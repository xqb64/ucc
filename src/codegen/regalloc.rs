use crate::{
    codegen::gen::{
        AsmFunction, AsmInstruction, AsmNode, AsmOperand, AsmProgram, AsmRegister, AsmSymtabEntry,
        AsmType, ASM_SYMBOL_TABLE,
    },
    ir::gen::make_temporary,
    util::cfg::{self, BasicBlock, Instr, SimpleInstr, CFG},
};
use std::collections::{BTreeMap, BTreeSet};

pub trait RegAlloc {
    fn reg_alloc(&mut self, aliased_pseudos: &BTreeSet<String>) -> Self;
}

impl RegAlloc for AsmProgram {
    fn reg_alloc(&mut self, aliased_pseudos: &BTreeSet<String>) -> Self {
        let mut functions = vec![];
        for func in &mut self.functions {
            functions.push(match func {
                AsmNode::Function(f) => AsmNode::Function(f.reg_alloc(aliased_pseudos)),
                _ => unreachable!(),
            });
        }
        AsmProgram {
            functions,
            static_constants: self.static_constants.clone(),
            static_vars: self.static_vars.clone(),
        }
    }
}

impl RegAlloc for AsmFunction {
    fn reg_alloc(&mut self, aliased_pseudos: &BTreeSet<String>) -> Self {
        let gp_graph = loop {
            let mut gp_graph = build_interference_graph(
                &self.name,
                aliased_pseudos,
                &self.instructions,
                &RegisterClass::GP,
            );

            let mut coalesced_regs =
                coalesce(&mut gp_graph, &self.instructions, &RegisterClass::GP);
            if coalesced_regs.nothing_was_coalesced() {
                break gp_graph;
            }

            self.instructions = rewrite_coalesced(&self.instructions, &mut coalesced_regs)
        };

        let gp_spilled_graph = add_spill_costs(gp_graph, &self.instructions);

        let colored_gp_graph =
            color_graph(gp_spilled_graph, GP_REGISTERS.len(), &RegisterClass::GP);

        let gp_register_map = make_register_map(&self.name, &colored_gp_graph, &RegisterClass::GP);

        let xmm_graph = loop {
            let mut xmm_graph = build_interference_graph(
                &self.name,
                aliased_pseudos,
                &self.instructions,
                &RegisterClass::XMM,
            );

            let mut coalesced_regs =
                coalesce(&mut xmm_graph, &self.instructions, &RegisterClass::XMM);
            if coalesced_regs.nothing_was_coalesced() {
                break xmm_graph;
            }

            self.instructions = rewrite_coalesced(&self.instructions, &mut coalesced_regs)
        };

        let xmm_spilled_graph = add_spill_costs(xmm_graph, &self.instructions);
        let colored_xmm_graph =
            color_graph(xmm_spilled_graph, XMM_REGISTERS.len(), &RegisterClass::XMM);
        let xmm_register_map =
            make_register_map(&self.name, &colored_xmm_graph, &RegisterClass::XMM);

        let mut register_map = gp_register_map;
        register_map.extend(xmm_register_map);

        let transformed_instructions = replace_pseudoregs(&self.instructions, &register_map);

        AsmFunction {
            name: self.name.clone(),
            instructions: transformed_instructions,
            global: self.global,
            stack_space: self.stack_space,
        }
    }
}

impl RegAlloc for AsmNode {
    fn reg_alloc(&mut self, aliased_pseudos: &BTreeSet<String>) -> Self {
        match self {
            AsmNode::Program(prog) => AsmNode::Program(prog.reg_alloc(aliased_pseudos)),
            AsmNode::Function(f) => AsmNode::Function(f.reg_alloc(aliased_pseudos)),
            _ => unreachable!(),
        }
    }
}

type NodeId = AsmOperand;

#[derive(Debug, Clone, PartialEq)]
struct Node {
    id: NodeId,
    neighbors: BTreeSet<NodeId>,
    spill_cost: f64,
    color: Option<usize>,
    pruned: bool,
}

type Graph = BTreeMap<NodeId, Node>;

fn mk_base_graph(all_hardregs: &BTreeSet<AsmOperand>) -> Graph {
    let mut graph = BTreeMap::new();

    for r in all_hardregs.iter() {
        let node = Node {
            id: r.clone(),
            neighbors: all_hardregs
                .iter()
                .filter(|reg| reg != &r)
                .map(|x| x.to_owned())
                .collect(),
            spill_cost: f64::INFINITY,
            color: None,
            pruned: false,
        };

        graph.insert(r.clone(), node);
    }

    graph
}

fn build_interference_graph(
    fn_name: &str,
    aliased_pseudos: &BTreeSet<String>,
    instructions: &[AsmInstruction],
    register_class: &RegisterClass,
) -> Graph {
    let all_hardregs = match register_class {
        RegisterClass::GP => GP_REGISTERS
            .iter()
            .map(|x| AsmOperand::Register(*x))
            .collect(),
        RegisterClass::XMM => XMM_REGISTERS
            .iter()
            .map(|x| AsmOperand::Register(*x))
            .collect(),
    };

    let mut graph = mk_base_graph(&all_hardregs);

    add_pseudo_nodes(&mut graph, aliased_pseudos, instructions, register_class);

    let cfg: CFG<(), AsmInstruction> =
        CFG::instructions_to_cfg("spam".to_string(), instructions.to_vec());
    let analyzed_cfg = LivenessAnalysis::analyze(fn_name, cfg, &all_hardregs, register_class);

    add_edges(&analyzed_cfg, &mut graph, register_class);

    graph
}

fn add_edge(graph: &mut Graph, nd_id1: &AsmOperand, nd_id2: &AsmOperand) {
    if let Some(nd1) = graph.get_mut(nd_id1) {
        nd1.neighbors.insert(nd_id2.clone());
    }

    if let Some(nd2) = graph.get_mut(nd_id2) {
        nd2.neighbors.insert(nd_id1.clone());
    }
}

fn add_edges(
    liveness_cfg: &CFG<BTreeSet<AsmOperand>, AsmInstruction>,
    interference_graph: &mut Graph,
    register_class: &RegisterClass,
) {
    for (_, block) in &liveness_cfg.basic_blocks {
        for (live_after_instr, instr) in &block.instructions {
            let (_unused, updated_regs) = regs_used_and_written(instr, register_class);

            let mut handle_livereg = |l: &AsmOperand| {
                let is_mov_src = matches!(instr,  AsmInstruction::Mov { src, .. } if src == l);

                if is_mov_src {
                    return;
                }

                for u in &updated_regs {
                    if u != l
                        && interference_graph.contains_key(l)
                        && interference_graph.contains_key(u)
                    {
                        add_edge(interference_graph, l, u);
                    }
                }
            };

            for live_reg in live_after_instr {
                handle_livereg(live_reg);
            }
        }
    }
}

type OperandSet = BTreeSet<AsmOperand>;

fn regs_used_and_written(
    instr: &AsmInstruction,
    register_class: &RegisterClass,
) -> (OperandSet, OperandSet) {
    let (ops_used, ops_written) = match instr {
        AsmInstruction::Mov { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::Movsx { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::MovZeroExtend { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::Cvtsi2sd { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::Cvttsd2si { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::Binary { lhs, rhs, .. } => {
            (vec![lhs.clone(), rhs.clone()], vec![rhs.clone()])
        }
        AsmInstruction::Unary { operand, .. } => (vec![operand.clone()], vec![operand.clone()]),
        AsmInstruction::Cmp { lhs, rhs, .. } => (vec![lhs.clone(), rhs.clone()], vec![]),
        AsmInstruction::SetCC { operand, .. } => (vec![], vec![operand.clone()]),
        AsmInstruction::Push(operand) => (vec![operand.clone()], vec![]),
        AsmInstruction::Idiv { operand, .. } | AsmInstruction::Div { operand, .. } => (
            vec![
                operand.clone(),
                AsmOperand::Register(AsmRegister::AX),
                AsmOperand::Register(AsmRegister::DX),
            ],
            vec![
                AsmOperand::Register(AsmRegister::AX),
                AsmOperand::Register(AsmRegister::DX),
            ],
        ),
        AsmInstruction::Cdq { .. } => (
            vec![AsmOperand::Register(AsmRegister::AX)],
            vec![AsmOperand::Register(AsmRegister::DX)],
        ),
        AsmInstruction::Call(func_name) => {
            let all_hardregs = match register_class {
                RegisterClass::GP => GP_REGISTERS,
                RegisterClass::XMM => XMM_REGISTERS,
            };

            let used_regs = {
                let table = ASM_SYMBOL_TABLE.lock().unwrap();
                if let Some(entry) = table.get(func_name) {
                    match entry {
                        AsmSymtabEntry::Function { param_regs, .. } => param_regs
                            .iter()
                            .filter(|r| all_hardregs.contains(r))
                            .cloned()
                            .map(AsmOperand::Register)
                            .collect::<Vec<_>>(),
                        _ => vec![],
                    }
                } else {
                    vec![]
                }
            };

            let regs = match register_class {
                RegisterClass::GP => GP_CALLER_SAVED_REGISTERS,
                RegisterClass::XMM => XMM_CALLER_SAVED_REGISTERS,
            };

            (
                used_regs,
                regs.iter().map(|r| AsmOperand::Register(*r)).collect(),
            )
        }
        AsmInstruction::Lea { src, dst } => (vec![src.clone()], vec![dst.clone()]),
        AsmInstruction::Jmp { .. }
        | AsmInstruction::JmpCC { .. }
        | AsmInstruction::Label(_)
        | AsmInstruction::Ret => (vec![], vec![]),
        _ => (vec![], vec![]),
    };

    let regs_used_to_read = |opr: &AsmOperand| -> Vec<AsmOperand> {
        match opr {
            AsmOperand::Pseudo(_) | AsmOperand::Register(_) => vec![opr.clone()],
            AsmOperand::Memory(base, _) => vec![AsmOperand::Register(*base)],
            AsmOperand::Indexed(base, index, _) => {
                vec![AsmOperand::Register(*base), AsmOperand::Register(*index)]
            }
            AsmOperand::Imm(_) | AsmOperand::Data(_, _) | AsmOperand::PseudoMem(_, _) => vec![],
        }
    };

    let regs_used_to_update = |opr: &AsmOperand| -> (Vec<AsmOperand>, Vec<AsmOperand>) {
        match opr {
            AsmOperand::Pseudo(_) | AsmOperand::Register(_) => (vec![], vec![opr.clone()]),
            AsmOperand::Memory(base, _) => (vec![AsmOperand::Register(*base)], vec![]),
            AsmOperand::Indexed(base, index, _) => (
                vec![AsmOperand::Register(*base), AsmOperand::Register(*index)],
                vec![],
            ),
            AsmOperand::Imm(_) | AsmOperand::Data(_, _) | AsmOperand::PseudoMem(_, _) => {
                (vec![], vec![])
            }
        }
    };

    let regs_read1: Vec<AsmOperand> = ops_used.iter().flat_map(regs_used_to_read).collect();

    let (regs_read2, regs_written): (Vec<_>, Vec<_>) =
        ops_written.iter().map(regs_used_to_update).unzip();

    let regs_read2 = regs_read2.into_iter().flatten().collect::<Vec<_>>();
    let regs_written = regs_written.into_iter().flatten().collect::<Vec<_>>();

    let regs_read = [regs_read1, regs_read2].concat();

    (
        regs_read.into_iter().collect(),
        regs_written.into_iter().collect(),
    )
}

struct LivenessAnalysis;

impl LivenessAnalysis {
    fn meet(
        fn_name: &str,
        cfg: &CFG<BTreeSet<AsmOperand>, AsmInstruction>,
        block: &BasicBlock<BTreeSet<AsmOperand>, AsmInstruction>,
        all_hardregs: &BTreeSet<AsmOperand>,
    ) -> OperandSet {
        let mut live_at_exit = OperandSet::new();

        let all_return_regs = match ASM_SYMBOL_TABLE.lock().unwrap().get(fn_name).unwrap() {
            AsmSymtabEntry::Function { return_regs, .. } => return_regs
                .iter()
                .map(|r| AsmOperand::Register(*r))
                .collect::<OperandSet>(),
            _ => unreachable!(),
        };
        let return_regs = all_hardregs
            .intersection(&all_return_regs)
            .cloned()
            .collect::<OperandSet>();

        for succ in &block.succs {
            match succ {
                cfg::NodeId::Exit => live_at_exit.extend(return_regs.clone()),
                cfg::NodeId::Entry => panic!("Internal error: malformed interference graph"),
                cfg::NodeId::Block(id) => {
                    let succ_live_registers = cfg.get_block_value(*id);
                    live_at_exit = live_at_exit.union(succ_live_registers).cloned().collect();
                }
            }
        }

        live_at_exit
    }

    fn transfer(
        block: &BasicBlock<BTreeSet<AsmOperand>, AsmInstruction>,
        end_live_regs: OperandSet,
        register_class: &RegisterClass,
    ) -> BasicBlock<BTreeSet<AsmOperand>, AsmInstruction> {
        let mut current_live_regs = end_live_regs.clone();
        let mut annotated_instructions = Vec::new();

        for (_, instr) in block.instructions.iter().enumerate().rev() {
            annotated_instructions.push((current_live_regs.clone(), instr.1.clone()));

            let (regs_used, regs_written) = regs_used_and_written(&instr.1, register_class);

            let without_killed: BTreeSet<_> = current_live_regs
                .difference(&regs_written)
                .cloned()
                .collect();
            current_live_regs = without_killed.union(&regs_used).cloned().collect();
        }

        BasicBlock {
            instructions: annotated_instructions.into_iter().rev().collect(),
            value: current_live_regs,
            ..block.clone()
        }
    }

    fn analyze(
        fn_name: &str,
        cfg: cfg::CFG<(), AsmInstruction>,
        all_hardregs: &BTreeSet<AsmOperand>,
        register_class: &RegisterClass,
    ) -> cfg::CFG<BTreeSet<AsmOperand>, AsmInstruction> {
        let mut starting_cfg = cfg.initialize_annotation(BTreeSet::new());

        let mut worklist: Vec<(usize, BasicBlock<BTreeSet<AsmOperand>, AsmInstruction>)> =
            starting_cfg.basic_blocks.clone();

        while let Some((block_idx, blk)) = worklist.pop() {
            let old_annotation = blk.value.clone();

            let live_vars_at_exit = Self::meet(fn_name, &starting_cfg, &blk, all_hardregs);

            let block = Self::transfer(&blk, live_vars_at_exit, register_class);

            starting_cfg.update_basic_block(block_idx, block.clone());

            let new_annotation = starting_cfg.get_block_value(block_idx);

            if old_annotation != *new_annotation {
                let block_predecessors = starting_cfg.get_preds(&blk.id);

                for pred in block_predecessors {
                    match pred {
                        cfg::NodeId::Block(_) => {
                            let pred_block = starting_cfg.get_block_by_id(&pred);
                            if !worklist.contains(&pred_block) {
                                worklist.push(pred_block.clone());
                            }
                        }
                        cfg::NodeId::Entry => continue,
                        cfg::NodeId::Exit => panic!("Internal error: malformed CFG"),
                    }
                }
            }
        }

        starting_cfg
    }
}

fn get_operands(instr: &AsmInstruction) -> Vec<AsmOperand> {
    match instr {
        AsmInstruction::Mov { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Movsx { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::MovZeroExtend { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Lea { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Cvttsd2si { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Cvtsi2sd { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Unary { operand, .. } => vec![operand.clone()],
        AsmInstruction::Binary { lhs, rhs, .. } => vec![lhs.clone(), rhs.clone()],
        AsmInstruction::Cmp { lhs, rhs, .. } => vec![lhs.clone(), rhs.clone()],
        AsmInstruction::Idiv { operand, .. } => vec![operand.clone()],
        AsmInstruction::Div { operand, .. } => vec![operand.clone()],
        AsmInstruction::SetCC { operand, .. } => vec![operand.clone()],
        AsmInstruction::Push(operand) => vec![operand.clone()],
        _ => vec![],
    }
}

fn pseudo_is_current_type(pseudo: &str, register_class: &RegisterClass) -> bool {
    if register_class == &RegisterClass::GP {
        match ASM_SYMBOL_TABLE.lock().unwrap().get(pseudo).unwrap() {
            AsmSymtabEntry::Object {
                _type,
                is_static: _,
                is_constant: _,
            } => _type != &AsmType::Double,
            _ => false,
        }
    } else if register_class == &RegisterClass::XMM {
        match ASM_SYMBOL_TABLE.lock().unwrap().get(pseudo).unwrap() {
            AsmSymtabEntry::Object {
                _type,
                is_static: _,
                is_constant: _,
            } => {
                return _type == &AsmType::Double;
            }
            _ => false,
        }
    } else {
        false
    }
}

fn get_pseudo_nodes(
    aliased_pseudos: &BTreeSet<String>,
    instructions: &[AsmInstruction],
    register_class: &RegisterClass,
) -> Vec<Node> {
    fn initialize_node(pseudo: String) -> Node {
        Node {
            id: NodeId::Pseudo(pseudo),
            neighbors: OperandSet::new(),
            spill_cost: 0.0,
            color: None,
            pruned: false,
        }
    }

    fn is_static(pseudo: &str) -> bool {
        match ASM_SYMBOL_TABLE.lock().unwrap().get(pseudo).unwrap() {
            AsmSymtabEntry::Object {
                _type,
                is_static,
                is_constant: _,
            } => *is_static,
            _ => false,
        }
    }

    fn pseudo_is_valid(
        pseudo: &String,
        register_class: &RegisterClass,
        aliased_pseudos: &BTreeSet<String>,
    ) -> bool {
        pseudo_is_current_type(pseudo, register_class)
            && !is_static(pseudo)
            && !aliased_pseudos.contains(pseudo)
    }

    let mut pseudos: Vec<String> = instructions
        .iter()
        .flat_map(|instr| {
            get_operands(instr)
                .iter()
                .filter_map(|op| match op {
                    AsmOperand::Pseudo(r)
                        if pseudo_is_valid(r, register_class, aliased_pseudos) =>
                    {
                        Some(r.clone())
                    }
                    _ => None,
                })
                .collect::<Vec<_>>()
        })
        .collect();

    pseudos.sort();
    pseudos.dedup();

    pseudos.into_iter().map(initialize_node).collect()
}

fn add_pseudo_nodes(
    graph: &mut Graph,
    aliased_pseudos: &BTreeSet<String>,
    instructions: &[AsmInstruction],
    register_class: &RegisterClass,
) {
    let pseudo_nodes = get_pseudo_nodes(aliased_pseudos, instructions, register_class);

    for node in pseudo_nodes {
        graph.insert(node.id.clone(), node);
    }
}

impl std::fmt::Display for AsmOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AsmOperand::Register(reg) => {
                let mut s = format!("{}", reg);
                s = s.replace("%", "");
                write!(f, "{}", s)
            }
            AsmOperand::Pseudo(pseudo) => write!(f, "{}", pseudo),
            AsmOperand::Memory(base, offset) => write!(f, "Memory({}, {})", base, offset),
            AsmOperand::Indexed(base, index, scale) => {
                write!(f, "Indexed({}, {}, {})", base, index, scale)
            }
            AsmOperand::Imm(imm) => write!(f, "Imm({})", imm),
            AsmOperand::Data(data, size) => write!(f, "Data({}, {})", data, size),
            AsmOperand::PseudoMem(name, offset) => write!(f, "PseudoMem({}, {})", name, offset),
        }
    }
}

#[allow(dead_code)]
fn print_graphviz(fn_name: &str, graph: &Graph) {
    use std::io::Write;

    let tmp = make_temporary();
    let dot_filename = format!("{}.{}.dot", fn_name, tmp);
    let png_filename = format!("{}.{}.png", fn_name, tmp);

    let mut file = match std::fs::File::create(&dot_filename) {
        Ok(f) => f,
        Err(_) => {
            return;
        }
    };

    writeln!(file, "graph {} {{", fn_name).expect("Failed to write to DOT file");

    for node in graph.values() {
        let node_id = &node.id;

        writeln!(file, "  \"{}\";", node_id).expect("Failed to write node to DOT file");

        for neighbor in &node.neighbors {
            writeln!(file, " \"{}\" -- \"{}\";", node_id, neighbor)
                .expect("Failed to write edge to DOT file");
        }
    }

    writeln!(file, "}}").expect("Failed to write closing brace to DOT file");

    let output = std::process::Command::new("dot")
        .arg(&dot_filename)
        .arg("-Tpng")
        .arg("-o")
        .arg(&png_filename)
        .output();

    match output {
        Ok(o) if o.status.success() => {
            println!("Graphviz output successfully written to: {}", png_filename);
        }
        Ok(o) => {
            eprintln!("Graphviz failed with error: {:?}", o);
        }
        Err(e) => {
            eprintln!("Failed to execute Graphviz command: {}", e);
        }
    }
}

impl Instr for AsmInstruction {
    fn simplify(&self) -> SimpleInstr {
        match self {
            AsmInstruction::Label(lbl) => SimpleInstr::Label(lbl.clone()),
            AsmInstruction::JmpCC { target, .. } => SimpleInstr::ConditionalJump(target.clone()),
            AsmInstruction::Jmp { target } => SimpleInstr::UnconditionalJump(target.clone()),
            AsmInstruction::Ret => SimpleInstr::Return,
            _ => SimpleInstr::Other,
        }
    }

    fn pp_instr(&self) -> String {
        match self {
            AsmInstruction::Label(lbl) => format!("Label({})", lbl),
            AsmInstruction::JmpCC { target, .. } => format!("ConditionalJump({})", target),
            AsmInstruction::Jmp { target } => format!("UnconditionalJump({})", target),
            AsmInstruction::Ret => "Return".to_string(),
            _ => format!("{:?}", self),
        }
    }

    fn is_jump(&self) -> bool {
        matches!(
            self,
            AsmInstruction::Jmp { .. } | AsmInstruction::JmpCC { .. }
        )
    }

    fn is_label(&self) -> bool {
        matches!(self, AsmInstruction::Label(_))
    }
}

fn add_spill_costs(
    graph: BTreeMap<NodeId, Node>,
    instructions: &[AsmInstruction],
) -> BTreeMap<NodeId, Node> {
    let incr_count = |counts: &mut BTreeMap<String, usize>, pseudo: &str| {
        *counts.entry(pseudo.to_string()).or_insert(0) += 1;
    };

    let operands: Vec<AsmOperand> = instructions.iter().flat_map(get_operands).collect();

    let pseudos: Vec<String> = operands
        .into_iter()
        .filter_map(|operand| match operand {
            AsmOperand::Pseudo(r) => Some(r),
            _ => None,
        })
        .collect();

    let mut count_map: BTreeMap<String, usize> = BTreeMap::new();
    for pseudo in pseudos {
        incr_count(&mut count_map, &pseudo);
    }

    graph
        .into_iter()
        .map(|(node_id, mut node)| {
            if let NodeId::Pseudo(ref r) = node.id {
                if let Some(&count) = count_map.get(r) {
                    node.spill_cost = count as f64;
                }
            }
            (node_id, node)
        })
        .collect()
}

use std::collections::VecDeque;

fn color_graph(mut graph: Graph, max_colors: usize, register_class: &RegisterClass) -> Graph {
    let mut stack: VecDeque<NodeId> = VecDeque::new();

    while !graph.values().all(|nd| nd.pruned) {
        let node_opt = graph
            .values()
            .find(|nd| {
                !nd.pruned
                    && nd
                        .neighbors
                        .iter()
                        .filter(|n| !graph.get(*n).unwrap().pruned)
                        .count()
                        < max_colors
            })
            .cloned();

        match node_opt {
            Some(node) => {
                graph.get_mut(&node.id).unwrap().pruned = true;
                stack.push_back(node.id.clone());
            }
            None => {
                let spill_node = graph
                    .values()
                    .filter(|nd| !nd.pruned)
                    .min_by(|a, b| {
                        let a_degree = a
                            .neighbors
                            .iter()
                            .filter(|n| !graph.get(*n).unwrap().pruned)
                            .count();
                        let b_degree = b
                            .neighbors
                            .iter()
                            .filter(|n| !graph.get(*n).unwrap().pruned)
                            .count();
                        (a.spill_cost / a_degree as f64)
                            .partial_cmp(&(b.spill_cost / b_degree as f64))
                            .unwrap_or(std::cmp::Ordering::Equal)
                    })
                    .cloned()
                    .expect("No nodes available to spill");

                graph.get_mut(&spill_node.id).unwrap().pruned = true;
                stack.push_back(spill_node.id.clone());
            }
        }
    }

    while let Some(node_id) = stack.pop_back() {
        let node = graph.get(&node_id).unwrap();
        let mut used_colors = BTreeSet::new();

        for neighbor_id in &node.neighbors {
            if let Some(neighbor) = graph.get(neighbor_id) {
                if let Some(color) = neighbor.color {
                    used_colors.insert(color);
                }
            }
        }

        let mut available_colors: Vec<usize> = (0..max_colors).collect();
        available_colors.retain(|c| !used_colors.contains(c));

        if !available_colors.is_empty() {
            if let NodeId::Register(reg) = &node_id {
                let caller_saved_regs = get_caller_saved_registers(register_class);

                if caller_saved_regs.contains(reg) {
                    graph.get_mut(&node_id).unwrap().color =
                        Some(*available_colors.iter().min().unwrap());
                } else {
                    graph.get_mut(&node_id).unwrap().color =
                        Some(*available_colors.iter().max().unwrap());
                }
            } else {
                graph.get_mut(&node_id).unwrap().color =
                    Some(*available_colors.iter().min().unwrap());
            }
        } else {
            graph.get_mut(&node_id).unwrap().color = None;
        }

        graph.get_mut(&node_id).unwrap().pruned = false;
    }

    graph
}

type IntMap = BTreeMap<usize, AsmRegister>;
type RegSet = BTreeSet<AsmRegister>;
type StringMap = BTreeMap<String, AsmRegister>;

fn make_register_map(
    fn_name: &str,
    graph: &BTreeMap<NodeId, Node>,
    register_class: &RegisterClass,
) -> BTreeMap<String, AsmRegister> {
    let mut colors_to_regs: IntMap = BTreeMap::new();

    for (nd_id, node) in graph.iter() {
        match nd_id {
            NodeId::Register(r) => {
                if let Some(color) = node.color {
                    colors_to_regs.insert(color, *r);
                }
            }
            _ => {}
        }
    }

    let mut used_callee_saved: RegSet = BTreeSet::new();
    let mut reg_map: StringMap = BTreeMap::new();

    for node in graph.values() {
        if let Node {
            id: NodeId::Pseudo(ref p),
            color: Some(c),
            ..
        } = node
        {
            if let Some(hardreg) = colors_to_regs.get(c) {
                let regs = get_caller_saved_registers(register_class);
                if !regs.contains(hardreg) {
                    used_callee_saved.insert(*hardreg);
                }

                reg_map.insert(p.clone(), *hardreg);
            }
        }
    }

    if let Some(AsmSymtabEntry::Function {
        ref mut callee_saved_regs_used,
        ..
    }) = ASM_SYMBOL_TABLE.lock().unwrap().get_mut(fn_name)
    {
        callee_saved_regs_used.extend(used_callee_saved);
    }

    reg_map
}

fn replace_pseudoregs(
    instructions: &[AsmInstruction],
    reg_map: &BTreeMap<String, AsmRegister>,
) -> Vec<AsmInstruction> {
    let replace_op = |op: AsmOperand| -> AsmOperand {
        match op {
            AsmOperand::Pseudo(ref p) => {
                if let Some(&hardreg) = reg_map.get(p) {
                    AsmOperand::Register(hardreg)
                } else {
                    op
                }
            }
            _ => op,
        }
    };

    cleanup_movs(
        instructions
            .iter()
            .map(|instr| replace_ops(replace_op, instr.to_owned()))
            .collect(),
    )
}

fn cleanup_movs(instructions: Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    let is_redundant_mov = |instr: &AsmInstruction| -> bool {
        matches!(instr, AsmInstruction::Mov { src, dst, .. } if src == dst)
    };

    instructions
        .into_iter()
        .filter(|instr| !is_redundant_mov(instr))
        .collect()
}

fn replace_ops<F>(mut f: F, instr: AsmInstruction) -> AsmInstruction
where
    F: FnMut(AsmOperand) -> AsmOperand,
{
    match instr {
        AsmInstruction::Mov { asm_type, src, dst } => AsmInstruction::Mov {
            asm_type,
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::Movsx {
            src,
            src_type,
            dst,
            dst_type,
        } => AsmInstruction::Movsx {
            src_type,
            src: f(src),
            dst_type,
            dst: f(dst),
        },
        AsmInstruction::MovZeroExtend {
            src,
            src_type,
            dst,
            dst_type,
        } => AsmInstruction::MovZeroExtend {
            src_type,
            src: f(src),
            dst_type,
            dst: f(dst),
        },
        AsmInstruction::Lea { src, dst } => AsmInstruction::Lea {
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::Cvttsd2si { asm_type, src, dst } => AsmInstruction::Cvttsd2si {
            asm_type,
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::Cvtsi2sd { asm_type, src, dst } => AsmInstruction::Cvtsi2sd {
            asm_type,
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::Unary {
            op,
            asm_type,
            operand,
        } => AsmInstruction::Unary {
            op,
            asm_type,
            operand: f(operand),
        },
        AsmInstruction::Binary {
            op,
            asm_type,
            lhs,
            rhs,
        } => AsmInstruction::Binary {
            asm_type,
            op,
            lhs: f(lhs),
            rhs: f(rhs),
        },
        AsmInstruction::Cmp { asm_type, lhs, rhs } => AsmInstruction::Cmp {
            asm_type,
            lhs: f(lhs),
            rhs: f(rhs),
        },
        AsmInstruction::Idiv { asm_type, operand } => AsmInstruction::Idiv {
            asm_type,
            operand: f(operand),
        },
        AsmInstruction::Div { asm_type, operand } => AsmInstruction::Div {
            asm_type,
            operand: f(operand),
        },
        AsmInstruction::SetCC { condition, operand } => AsmInstruction::SetCC {
            condition,
            operand: f(operand),
        },
        AsmInstruction::Push(v) => AsmInstruction::Push(f(v)),
        AsmInstruction::Label(_)
        | AsmInstruction::Call(_)
        | AsmInstruction::Ret
        | AsmInstruction::Cdq { .. }
        | AsmInstruction::Jmp { .. }
        | AsmInstruction::JmpCC { .. }
        | _ => instr,
    }
}

const GP_REGISTERS: &[AsmRegister] = &[
    AsmRegister::AX,
    AsmRegister::BX,
    AsmRegister::CX,
    AsmRegister::DX,
    AsmRegister::SI,
    AsmRegister::DI,
    AsmRegister::R8,
    AsmRegister::R9,
    AsmRegister::R12,
    AsmRegister::R13,
    AsmRegister::R14,
    AsmRegister::R15,
];

const XMM_REGISTERS: &[AsmRegister] = &[
    AsmRegister::XMM0,
    AsmRegister::XMM1,
    AsmRegister::XMM2,
    AsmRegister::XMM3,
    AsmRegister::XMM4,
    AsmRegister::XMM5,
    AsmRegister::XMM6,
    AsmRegister::XMM7,
    AsmRegister::XMM8,
    AsmRegister::XMM9,
    AsmRegister::XMM10,
    AsmRegister::XMM11,
    AsmRegister::XMM12,
    AsmRegister::XMM13,
];

const GP_CALLER_SAVED_REGISTERS: &[AsmRegister] = &[
    AsmRegister::AX,
    AsmRegister::CX,
    AsmRegister::DX,
    AsmRegister::SI,
    AsmRegister::DI,
    AsmRegister::R8,
    AsmRegister::R9,
];

const XMM_CALLER_SAVED_REGISTERS: &[AsmRegister] = &[
    AsmRegister::XMM0,
    AsmRegister::XMM1,
    AsmRegister::XMM2,
    AsmRegister::XMM3,
    AsmRegister::XMM4,
    AsmRegister::XMM5,
    AsmRegister::XMM6,
    AsmRegister::XMM7,
    AsmRegister::XMM8,
    AsmRegister::XMM9,
    AsmRegister::XMM10,
    AsmRegister::XMM11,
    AsmRegister::XMM12,
    AsmRegister::XMM13,
];

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum RegisterClass {
    GP,
    XMM,
}

fn get_caller_saved_registers(register_class: &RegisterClass) -> &[AsmRegister] {
    match register_class {
        RegisterClass::GP => GP_CALLER_SAVED_REGISTERS,
        RegisterClass::XMM => XMM_CALLER_SAVED_REGISTERS,
    }
}

#[derive(Debug, PartialEq, Clone)]
struct DisjointSet<T: Ord + Clone> {
    reg_map: BTreeMap<T, T>,
}

impl<T: Ord + Clone> DisjointSet<T> {
    pub fn new() -> Self {
        Self {
            reg_map: BTreeMap::new(),
        }
    }

    pub fn union(&mut self, x: T, y: T) {
        self.reg_map.insert(x, y);
    }

    pub fn find(&mut self, r: T) -> T {
        if let Some(parent) = self.reg_map.get(&r).cloned() {
            let root = self.find(parent.clone());
            self.reg_map.insert(r.clone(), root.clone());
            root
        } else {
            r
        }
    }

    pub fn nothing_was_coalesced(&self) -> bool {
        self.reg_map.is_empty()
    }
}

fn coalesce(
    graph: &mut Graph,
    instructions: &[AsmInstruction],
    register_class: &RegisterClass,
) -> DisjointSet<AsmOperand> {
    let mut coalesced_regs = DisjointSet::new();

    for i in instructions {
        match i {
            AsmInstruction::Mov {
                asm_type: _,
                src,
                dst,
            } => {
                let src = coalesced_regs.find(src.clone());
                let dst = coalesced_regs.find(dst.clone());

                if let Some(src_node) = graph.get(&src) {
                    if graph.get(&dst).is_some() {
                        if src != dst {
                            if !src_node.neighbors.contains(&dst)
                                && conservative_coalesceable(
                                    graph,
                                    src.clone(),
                                    dst.clone(),
                                    register_class,
                                )
                            {
                                let to_keep;
                                let to_merge;
                                if let AsmOperand::Register(reg) = src {
                                    if GP_REGISTERS.contains(&reg) || XMM_REGISTERS.contains(&reg) {
                                        to_keep = src;
                                        to_merge = dst;
                                    } else {
                                        to_keep = dst;
                                        to_merge = src;
                                    }
                                } else {
                                    to_keep = dst;
                                    to_merge = src;
                                }

                                coalesced_regs.union(to_merge.clone(), to_keep.clone());
                                update_graph(graph, to_merge, to_keep);
                            }
                        }
                    }
                }
            }
            _ => continue,
        }
    }

    coalesced_regs.clone()
}

fn rewrite_coalesced(
    instructions: &[AsmInstruction],
    coalesced_regs: &mut DisjointSet<AsmOperand>,
) -> Vec<AsmInstruction> {
    let mut rewrite_instruction = |instr: &AsmInstruction| -> Option<AsmInstruction> {
        match instr {
            AsmInstruction::Mov { asm_type, src, dst } => {
                let new_src = coalesced_regs.find(src.clone());
                let new_dst = coalesced_regs.find(dst.clone());

                if new_src == new_dst {
                    None
                } else {
                    Some(AsmInstruction::Mov {
                        asm_type: asm_type.to_owned(),
                        src: new_src,
                        dst: new_dst,
                    })
                }
            }

            _ => Some(replace_ops(
                |x| coalesced_regs.find(x.clone()),
                instr.clone(),
            )),
        }
    };

    instructions
        .into_iter()
        .filter_map(|x| rewrite_instruction(&x))
        .collect()
}

fn update_graph(graph: &mut Graph, x: AsmOperand, y: AsmOperand) {
    let node_to_remove = graph.get(&x).unwrap().clone();

    for neighbor in node_to_remove.neighbors.iter() {
        add_edge(graph, &y, neighbor);
        remove_edge(graph, &x, neighbor);
    }

    graph.remove(&x);
}

fn remove_edge(graph: &mut Graph, x: &AsmOperand, y: &AsmOperand) {
    let x_node = graph.get_mut(x).unwrap();
    x_node.neighbors.remove(y);
    let y_node = graph.get_mut(y).unwrap();
    y_node.neighbors.remove(x);
}

fn conservative_coalesceable(
    graph: &mut Graph,
    src: AsmOperand,
    dst: AsmOperand,
    register_class: &RegisterClass,
) -> bool {
    if briggs_test(graph, &src, &dst, register_class) {
        return true;
    }
    if let AsmOperand::Register(_) = src {
        return george_test(graph, src, dst, register_class);
    }
    if let AsmOperand::Register(_) = dst {
        return george_test(graph, dst, src, register_class);
    }
    false
}

fn briggs_test(
    graph: &mut Graph,
    src: &AsmOperand,
    dst: &AsmOperand,
    register_class: &RegisterClass,
) -> bool {
    let mut significant_neighbors = 0;

    let k = match register_class {
        RegisterClass::GP => GP_REGISTERS.len(),
        RegisterClass::XMM => XMM_REGISTERS.len(),
    };

    let x_node = graph.get(src).unwrap();
    let y_node = graph.get(dst).unwrap();

    let mut combined_neighbors = x_node.neighbors.iter().collect::<BTreeSet<_>>();
    combined_neighbors = combined_neighbors
        .union(&y_node.neighbors.iter().collect::<BTreeSet<_>>())
        .cloned()
        .collect::<BTreeSet<_>>();

    for n in combined_neighbors {
        let neighbor_node = graph.get(n).unwrap();
        let mut degree = neighbor_node.neighbors.len();

        if neighbor_node.neighbors.contains(src) && neighbor_node.neighbors.contains(dst) {
            degree -= 1;
        }

        if degree >= k {
            significant_neighbors += 1;
        }
    }

    significant_neighbors < k
}

fn george_test(
    graph: &mut Graph,
    hardreg: AsmOperand,
    pseudoreg: AsmOperand,
    register_class: &RegisterClass,
) -> bool {
    let pseudo_node = graph.get(&pseudoreg).unwrap();

    let k = match register_class {
        RegisterClass::GP => GP_REGISTERS.len(),
        RegisterClass::XMM => XMM_REGISTERS.len(),
    };

    for n in pseudo_node.neighbors.iter() {
        let neighbor_node = graph.get(n).unwrap();
        if neighbor_node.neighbors.contains(&hardreg) {
            continue;
        }

        if neighbor_node.neighbors.len() < k {
            continue;
        }

        return false;
    }

    true
}
