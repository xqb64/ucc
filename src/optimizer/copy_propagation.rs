use crate::ir::{IRInstruction, IRValue, get_dst};
use crate::codegen::tacky_type;
use crate::typechecker::{get_signedness, SYMBOL_TABLE, IdentifierAttrs};
use std::collections::BTreeSet;
use crate::cfg::{self, BasicBlock, NodeId};
use crate::lexer::Const;


#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Cp {
    src: IRValue,
    dst: IRValue,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ReachingCopies(BTreeSet<Cp>);

impl Default for ReachingCopies {
    fn default() -> Self {
        Self::new()
    }
}

impl ReachingCopies {
    pub fn new() -> Self {
        ReachingCopies(BTreeSet::new())
    }

    pub fn intersection(self, other: &ReachingCopies) -> ReachingCopies {
        let new_set = self.0.intersection(&other.0).cloned().collect();
        ReachingCopies(new_set)
    }

    pub fn add(mut self, copy: Cp) -> Self {
        self.0.insert(copy);
        self
    }

    pub fn mem(&self, copy: &Cp) -> bool {
        self.0.contains(copy)
    }

    pub fn filter<F>(self, f: F) -> Self
    where
        F: Fn(&Cp) -> bool,
    {
        ReachingCopies(self.0.iter().filter(|&cp| f(cp)).cloned().collect())
    }
}

fn same_type(v1: &IRValue, v2: &IRValue) -> bool {
    let t1 = tacky_type(v1);
    let t2 = tacky_type(v2);
    t1 == t2 || get_signedness(&t1) == get_signedness(&t2)
}

fn is_static(var: &str) -> bool {
    matches!(
        SYMBOL_TABLE.lock().unwrap().get(var).unwrap().attrs,
        IdentifierAttrs::StaticAttr { .. }
    )
}

fn var_is_aliased(aliased_vars: &BTreeSet<String>, v: &IRValue) -> bool {
    match v {
        IRValue::Constant(_) => false,
        IRValue::Var(var) => aliased_vars.contains(var) || is_static(var),
    }
}

fn filter_updated(copies: ReachingCopies, updated: &IRValue) -> ReachingCopies {
    let is_killed = |cp: &Cp| cp.src == *updated || cp.dst == *updated;
    copies.filter(|cp| !is_killed(cp))
}

fn transfer(
    aliased_vars: &BTreeSet<String>,
    block: &BasicBlock<ReachingCopies, IRInstruction>,
    initial_reaching_copies: ReachingCopies,
) -> BasicBlock<ReachingCopies, IRInstruction> {
    let is_aliased = |var: &IRValue| var_is_aliased(aliased_vars, var);

    let process_instr = |current_copies: ReachingCopies, instr: &IRInstruction| {
        let annotated_instr = (current_copies.clone(), instr.clone());

        let new_copies = match instr {
            IRInstruction::Copy { src, dst } => {
                if current_copies.mem(&Cp {
                    src: dst.clone(),
                    dst: src.clone(),
                }) {
                    current_copies
                } else if same_type(src, dst) {
                    let updated = filter_updated(current_copies, dst);

                    updated.add(Cp {
                        src: src.clone(),
                        dst: dst.clone(),
                    })
                } else {
                    filter_updated(current_copies, dst)
                }
            }
            IRInstruction::Call { dst, .. } => {
                let copies_after_dst_filter = match dst {
                    Some(d) => filter_updated(current_copies, d),
                    None => current_copies,
                };

                copies_after_dst_filter.filter(|cp| !(is_aliased(&cp.src) || is_aliased(&cp.dst)))
            }
            IRInstruction::Store { .. } => {
                current_copies.filter(|cp| !(is_aliased(&cp.src) || is_aliased(&cp.dst)))
            }
            _ => match get_dst(instr) {
                Some(dst) => filter_updated(current_copies, &dst),
                None => current_copies,
            },
        };

        (new_copies, annotated_instr)
    };

    let (final_reaching_copies, annotated_instructions): (
        ReachingCopies,
        Vec<(ReachingCopies, IRInstruction)>,
    ) = block.instructions.iter().fold(
        (initial_reaching_copies, Vec::new()),
        |(current_copies, mut annotated_instrs), instr| {
            let (new_copies, annotated_instr) = process_instr(current_copies, &instr.1);
            annotated_instrs.push(annotated_instr);
            (new_copies, annotated_instrs)
        },
    );

    BasicBlock {
        instructions: annotated_instructions,
        value: final_reaching_copies,
        ..block.clone()
    }
}

fn meet(
    ident: &ReachingCopies,
    cfg: &cfg::CFG<ReachingCopies, IRInstruction>,
    block: &BasicBlock<ReachingCopies, IRInstruction>,
) -> ReachingCopies {
    let mut incoming = ident.clone();
    for pred in &block.preds {
        match pred {
            NodeId::Entry => {
                return ReachingCopies::new();
            }
            NodeId::Block(n) => {
                let v = cfg.get_block_value(*n);
                println!("v: {:?}", v);
                println!("incoming: {:?}", incoming);
                incoming = incoming.intersection(v);
                println!("RESULT: {:?}", incoming);
            }
            _ => panic!("Internal error"),
        }
    }
    incoming
}

fn find_reaching_copies(
    aliased_vars: &BTreeSet<String>,
    cfg: cfg::CFG<(), IRInstruction>,
) -> cfg::CFG<ReachingCopies, IRInstruction> {
    let ident = collect_all_copies(&cfg);

    let mut starting_cfg = cfg.initialize_annotation(ident.clone());

    let mut worklist: Vec<(usize, BasicBlock<ReachingCopies, IRInstruction>)> =
        starting_cfg.basic_blocks.clone();

    loop {
        if worklist.is_empty() {
            break;
        }

        let (block_idx, blk) = worklist.remove(0);

        let old_annotation = blk.value.clone();

        let incoming_copies = meet(&ident, &starting_cfg, &blk);

        let block = transfer(aliased_vars, &blk, incoming_copies);

        starting_cfg.update_basic_block(block_idx, block);

        let new_annotation = starting_cfg.get_block_value(block_idx);
        if &old_annotation != new_annotation {
            let block_successors = starting_cfg.get_succs(&blk.id);

            for succ in block_successors {
                match succ {
                    NodeId::Block(_) => {
                        let s = starting_cfg.get_block_by_id(&succ);
                        if !worklist.contains(&s) {
                            worklist.push(s.clone());
                        }
                    }
                    NodeId::Exit => continue,
                    _ => panic!("Internal error"),
                }
            }
        }
    }

    starting_cfg
}

pub fn copy_propagation(
    aliased_vars: &BTreeSet<String>,
    cfg: cfg::CFG<(), IRInstruction>,
) -> cfg::CFG<(), IRInstruction> {
    let annotated_cfg = find_reaching_copies(aliased_vars, cfg);

    let rewrite_block = |block: BasicBlock<ReachingCopies, IRInstruction>| {
        let new_instructions = block
            .instructions
            .iter()
            .filter_map(|(reaching_copies, instr)| {
                rewrite_instruction(reaching_copies, instr)
                    .map(|new_instr| (reaching_copies.clone(), new_instr))
            })
            .collect::<Vec<_>>();

        BasicBlock {
            instructions: new_instructions,
            ..block.clone()
        }
    };

    let transformed_cfg = cfg::CFG {
        basic_blocks: annotated_cfg
            .basic_blocks
            .iter()
            .map(|(idx, blk)| (*idx, rewrite_block(blk.clone())))
            .collect(),
        entry_succs: annotated_cfg.entry_succs.clone(),
        exit_preds: annotated_cfg.exit_preds.clone(),
        debug_label: annotated_cfg.debug_label.clone(),
    };

    transformed_cfg.strip_annotations()
}

fn rewrite_instruction(
    reaching_copies: &ReachingCopies,
    instr: &IRInstruction,
) -> Option<IRInstruction> {
    if let IRInstruction::Copy { src, dst } = instr {
        if reaching_copies.mem(&Cp {
            src: src.clone(),
            dst: dst.clone(),
        }) || reaching_copies.mem(&Cp {
            src: dst.clone(),
            dst: src.clone(),
        }) {
            return None;
        }
    }

    let replace = |op: &IRValue| -> IRValue {
        match op {
            IRValue::Constant(_) => op.clone(),
            IRValue::Var(_) => reaching_copies
                .0
                .iter()
                .find(|cp| cp.dst == op.clone())
                .map(|cp| cp.src.clone())
                .unwrap_or(op.clone()),
        }
    };

    match instr {
        IRInstruction::Copy { src, dst } => Some(IRInstruction::Copy {
            src: replace(src),
            dst: dst.clone(),
        }),
        IRInstruction::Unary { op, src, dst } => Some(IRInstruction::Unary {
            src: replace(src),
            op: *op,
            dst: dst.clone(),
        }),
        IRInstruction::Binary { op, lhs, rhs, dst } => Some(IRInstruction::Binary {
            lhs: replace(lhs),
            rhs: replace(rhs),
            op: *op,
            dst: dst.clone(),
        }),
        IRInstruction::Ret(v) => Some(IRInstruction::Ret(v.as_ref().map(replace))),
        IRInstruction::JumpIfZero { condition, target } => Some(IRInstruction::JumpIfZero {
            condition: replace(condition),
            target: target.clone(),
        }),
        IRInstruction::JumpIfNotZero { condition, target } => Some(IRInstruction::JumpIfNotZero {
            condition: replace(condition),
            target: target.clone(),
        }),
        IRInstruction::SignExtend { src, dst } => Some(IRInstruction::SignExtend {
            src: replace(src),
            dst: dst.clone(),
        }),
        IRInstruction::ZeroExtend { src, dst } => Some(IRInstruction::ZeroExtend {
            src: replace(src),
            dst: dst.clone(),
        }),
        IRInstruction::Truncate { src, dst } => Some(IRInstruction::Truncate {
            src: replace(src),
            dst: dst.clone(),
        }),
        IRInstruction::IntToDouble { src, dst } => Some(IRInstruction::IntToDouble {
            src: replace(src),
            dst: dst.clone(),
        }),
        IRInstruction::DoubleToInt { src, dst } => Some(IRInstruction::DoubleToInt {
            src: replace(src),
            dst: dst.clone(),
        }),
        IRInstruction::DoubletoUInt { src, dst } => Some(IRInstruction::DoubletoUInt {
            src: replace(src),
            dst: dst.clone(),
        }),
        IRInstruction::UIntToDouble { src, dst } => Some(IRInstruction::UIntToDouble {
            src: replace(src),
            dst: dst.clone(),
        }),
        IRInstruction::CopyToOffset { src, dst, offset } => Some(IRInstruction::CopyToOffset {
            src: replace(src),
            dst: dst.clone(),
            offset: *offset,
        }),
        IRInstruction::CopyFromOffset { src, dst, offset } => {
            match replace(&IRValue::Var(src.to_owned())) {
                IRValue::Var(new_src) => Some(IRInstruction::CopyFromOffset {
                    src: new_src,
                    dst: dst.clone(),
                    offset: *offset,
                }),
                _ => panic!("internal error"),
            }
        }
        IRInstruction::AddPtr {
            ptr,
            index,
            scale,
            dst,
        } => Some(IRInstruction::AddPtr {
            ptr: replace(ptr),
            index: replace(index),
            scale: *scale,
            dst: dst.clone(),
        }),
        IRInstruction::Load { src_ptr, dst } => Some(IRInstruction::Load {
            src_ptr: replace(src_ptr),
            dst: dst.clone(),
        }),
        IRInstruction::Store { src, dst_ptr } => Some(IRInstruction::Store {
            src: replace(src),
            dst_ptr: dst_ptr.clone(),
        }),
        IRInstruction::Call { target, args, dst } => {
            let new_args = args.iter().map(replace).collect();
            Some(IRInstruction::Call {
                target: target.clone(),
                args: new_args,
                dst: dst.clone(),
            })
        }
        _ => Some(instr.clone()),
    }
}

fn collect_all_copies(cfg: &cfg::CFG<(), IRInstruction>) -> ReachingCopies {
    let mut copies = ReachingCopies::new();

    for (_, block) in &cfg.basic_blocks {
        for (_, instr) in &block.instructions {
            if let IRInstruction::Copy { src, dst } = instr {
                if same_type(src, dst) {
                    copies = copies.add(Cp {
                        src: src.clone(),
                        dst: dst.clone(),
                    });
                }
            }
        }
    }

    copies
}

impl std::fmt::Debug for ReachingCopies {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::fmt::Debug for Cp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn extract(v: &IRValue) -> String {
            match v {
                IRValue::Constant(c) => match c {
                    Const::Int(i) => i.to_string(),
                    Const::Long(l) => l.to_string(),
                    Const::UInt(u) => u.to_string(),
                    Const::ULong(ul) => ul.to_string(),
                    Const::Double(d) => d.to_string(),
                    Const::Char(c) => c.to_string(),
                    Const::UChar(uc) => uc.to_string(),
                },
                IRValue::Var(v) => v.to_owned(),
            }
        }

        write!(f, "{:?} = {:?}", extract(&self.dst), extract(&self.src))
    }
}
