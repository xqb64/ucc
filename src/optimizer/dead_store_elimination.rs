use crate::cfg::{self, BasicBlock, NodeId};
use crate::ir::{get_dst, IRInstruction, IRValue};
use crate::typechecker::{IdentifierAttrs, SYMBOL_TABLE};
use std::collections::BTreeSet;

pub fn dead_store_elimination(
    aliased_vars: &BTreeSet<String>,
    cfg: cfg::CFG<(), IRInstruction>,
) -> cfg::CFG<(), IRInstruction> {
    let mut static_vars = BTreeSet::new();

    for (k, v) in SYMBOL_TABLE.lock().unwrap().iter() {
        if let IdentifierAttrs::StaticAttr { .. } = v.attrs {
            static_vars.insert(k.to_owned());
        }
    }

    let annotated_cfg = find_live_variables(&static_vars, aliased_vars, cfg);

    let rewrite_block = |(idx, block): (usize, BasicBlock<BTreeSet<String>, IRInstruction>)| -> (usize, BasicBlock<BTreeSet<String>, IRInstruction>) {
        let new_instructions = block
            .instructions
            .into_iter()
            .filter(|instr| !is_dead_store(instr))
            .collect();

        (
            idx,
            BasicBlock {
                instructions: new_instructions,
                preds: block.preds,
                succs: block.succs,
                value: block.value,
                id: block.id,
            },
        )
    };

    cfg::CFG {
        basic_blocks: annotated_cfg
            .basic_blocks
            .into_iter()
            .map(rewrite_block)
            .collect(),
        ..annotated_cfg
    }
    .strip_annotations()
}

fn is_dead_store((live_vars, i): &(BTreeSet<String>, IRInstruction)) -> bool {
    match i {
        IRInstruction::Call { .. } | IRInstruction::Store { .. } => false,
        _ => match get_dst(i) {
            Some(IRValue::Var(v)) => !live_vars.contains(&v),
            _ => false,
        },
    }
}

fn find_live_variables(
    static_vars: &BTreeSet<String>,
    aliased_vars: &BTreeSet<String>,
    cfg: cfg::CFG<(), IRInstruction>,
) -> cfg::CFG<BTreeSet<String>, IRInstruction> {
    let mut starting_cfg = cfg.initialize_annotation(BTreeSet::new());

    let static_and_aliased_vars = static_vars
        .union(aliased_vars)
        .cloned()
        .collect::<BTreeSet<_>>();

    let mut worklist: Vec<(usize, BasicBlock<BTreeSet<String>, IRInstruction>)> =
        starting_cfg.basic_blocks.clone();

    while let Some((block_idx, blk)) = worklist.pop() {
        let old_annotation = blk.value.clone();

        let live_vars_at_exit = meet_dead_store(static_vars, &starting_cfg, &blk);

        let block = transfer_dead_store(&static_and_aliased_vars, blk.clone(), live_vars_at_exit);

        starting_cfg.update_basic_block(block_idx, block.clone());

        let new_annotation = starting_cfg.get_block_value(block_idx);

        if old_annotation != *new_annotation {
            let block_predecessors = starting_cfg.get_preds(&blk.id);

            for pred in block_predecessors {
                match pred {
                    NodeId::Block(_) => {
                        let pred_block = starting_cfg.get_block_by_id(&pred);
                        if !worklist.contains(&pred_block) {
                            worklist.push(pred_block.clone());
                        }
                    }
                    NodeId::Entry => continue,
                    NodeId::Exit => panic!("Internal error: malformed CFG"),
                }
            }
        }
    }

    starting_cfg
}

fn meet_dead_store(
    static_vars: &BTreeSet<String>,
    cfg: &cfg::CFG<BTreeSet<String>, IRInstruction>,
    block: &BasicBlock<BTreeSet<String>, IRInstruction>,
) -> BTreeSet<String> {
    let mut live = BTreeSet::new();

    for succ in &block.succs {
        match succ {
            NodeId::Entry => panic!("Internal error: malformed CFG"),
            NodeId::Exit => {
                live = live.union(static_vars).cloned().collect();
            }
            NodeId::Block(n) => {
                live = live.union(cfg.get_block_value(*n)).cloned().collect();
            }
        };
    }

    live
}

fn transfer_dead_store(
    static_and_aliased_vars: &BTreeSet<String>,
    block: BasicBlock<BTreeSet<String>, IRInstruction>,
    end_live_variables: BTreeSet<String>,
) -> BasicBlock<BTreeSet<String>, IRInstruction> {
    let process_instr = |mut current_live_vars: BTreeSet<String>,
                         (_, i): &(BTreeSet<String>, IRInstruction)|
     -> (BTreeSet<String>, (BTreeSet<String>, IRInstruction)) {
        let annotated_instr = (current_live_vars.clone(), i.clone());

        match i {
            IRInstruction::Binary {
                op: _,
                lhs,
                rhs,
                dst,
            } => {
                current_live_vars.remove(&dst.to_string());
                current_live_vars.insert(lhs.to_string());
                current_live_vars.insert(rhs.to_string());
            }
            IRInstruction::Unary { op: _, dst, src } => {
                current_live_vars.remove(&dst.to_string());
                current_live_vars.insert(src.to_string());
            }
            IRInstruction::JumpIfZero {
                target: _,
                condition,
            }
            | IRInstruction::JumpIfNotZero {
                target: _,
                condition,
            } => {
                current_live_vars.insert(condition.to_string());
            }
            IRInstruction::Copy { dst, src } => {
                current_live_vars.remove(&dst.to_string());
                current_live_vars.insert(src.to_string());
            }
            IRInstruction::Ret(Some(v)) => {
                current_live_vars.insert(v.to_string());
            }
            IRInstruction::Call {
                dst,
                args,
                target: _,
            } => {
                if let Some(d) = dst {
                    current_live_vars.remove(&d.to_string());
                }
                for arg in args {
                    current_live_vars.insert(arg.to_string());
                }
                current_live_vars = current_live_vars
                    .union(static_and_aliased_vars)
                    .cloned()
                    .collect();
            }
            IRInstruction::SignExtend { dst, src }
            | IRInstruction::ZeroExtend { dst, src }
            | IRInstruction::DoubleToInt { dst, src }
            | IRInstruction::IntToDouble { dst, src }
            | IRInstruction::DoubletoUInt { dst, src }
            | IRInstruction::UIntToDouble { dst, src }
            | IRInstruction::Truncate { dst, src } => {
                current_live_vars.remove(&dst.to_string());
                current_live_vars.insert(src.to_string());
            }
            IRInstruction::AddPtr {
                dst,
                ptr,
                index,
                scale: _,
            } => {
                current_live_vars.remove(&dst.to_string());
                current_live_vars.insert(ptr.to_string());
                current_live_vars.insert(index.to_string());
            }
            IRInstruction::GetAddress { src: _, dst } => {
                current_live_vars.remove(&dst.to_string());
            }
            IRInstruction::Load { src_ptr, dst } => {
                current_live_vars.remove(&dst.to_string());
                current_live_vars.insert(src_ptr.to_string());
                current_live_vars = current_live_vars
                    .union(static_and_aliased_vars)
                    .cloned()
                    .collect();
            }
            IRInstruction::Store { src, dst_ptr } => {
                current_live_vars.insert(src.to_string());
                current_live_vars.insert(dst_ptr.to_string());
            }
            IRInstruction::CopyToOffset {
                src,
                dst: _,
                offset: _,
            } => {
                current_live_vars.insert(src.to_string());
            }
            IRInstruction::CopyFromOffset {
                src,
                dst,
                offset: _,
            } => {
                current_live_vars.remove(&dst.to_string());
                current_live_vars.insert(src.to_string());
            }
            _ => {}
        }

        (current_live_vars.clone(), annotated_instr.clone())
    };

    let (incoming_live_vars, annotated_reversed_instructions): (
        BTreeSet<String>,
        Vec<(BTreeSet<String>, IRInstruction)>,
    ) = block.instructions.iter().rev().fold(
        (end_live_variables, Vec::new()),
        |(current_copies, mut annotated_instrs), instr| {
            let (new_live_vars, annotated_instr) = process_instr(current_copies, instr);
            annotated_instrs.push(annotated_instr);
            (new_live_vars, annotated_instrs)
        },
    );

    BasicBlock {
        instructions: annotated_reversed_instructions.into_iter().rev().collect(),
        value: incoming_live_vars,
        ..block.clone()
    }
}
