use crate::ir::{make_temporary, IRInstruction};

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum NodeId {
    Entry,
    Exit,
    BlockId(usize),
}

impl Ord for NodeId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (NodeId::Entry, NodeId::Entry) => std::cmp::Ordering::Equal,
            (NodeId::Exit, NodeId::Exit) => std::cmp::Ordering::Equal,
            (NodeId::BlockId(a), NodeId::BlockId(b)) => a.cmp(b),
            (NodeId::Entry, _) => std::cmp::Ordering::Less,
            (_, NodeId::Entry) => std::cmp::Ordering::Greater,
            (NodeId::Exit, _) => std::cmp::Ordering::Greater,
            (_, NodeId::Exit) => std::cmp::Ordering::Less,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Node {
    Entry {
        successors: Vec<NodeId>,
    },
    Exit {
        predecessors: Vec<NodeId>,
    },
    Block {
        id: NodeId,
        predecessors: Vec<NodeId>,
        instructions: Vec<IRInstruction>,
        successors: Vec<NodeId>,
    },
}

impl Node {
    pub fn id(&self) -> NodeId {
        match self {
            Node::Entry { .. } => NodeId::Entry,
            Node::Exit { .. } => NodeId::Exit,
            Node::Block { id, .. } => id.to_owned(),
        }
    }
}

impl Eq for Node {}

fn partition_into_basic_blocks(instructions: &[IRInstruction]) -> Vec<Vec<&IRInstruction>> {
    let mut finished_blocks = vec![];
    let mut current_block = vec![];

    for instruction in instructions {
        match instruction {
            IRInstruction::Label { .. } => {
                if !current_block.is_empty() {
                    finished_blocks.push(current_block);
                }
                current_block = vec![instruction];
            }
            IRInstruction::Jump(_)
            | IRInstruction::JumpIfZero { .. }
            | IRInstruction::JumpIfNotZero { .. }
            | IRInstruction::Ret(_) => {
                current_block.push(instruction);
                finished_blocks.push(current_block);
                current_block = vec![];
            }
            _ => {
                current_block.push(instruction);
            }
        }
    }

    if !current_block.is_empty() {
        finished_blocks.push(current_block);
    }

    finished_blocks
}

fn create_basic_blocks(instruction_groups: Vec<Vec<&IRInstruction>>) -> Vec<Node> {
    let mut blocks = vec![];
    let mut block_id_counter = 0;

    for group in instruction_groups {
        blocks.push(Node::Block {
            id: NodeId::BlockId(block_id_counter),
            predecessors: vec![],
            instructions: group.iter().cloned().cloned().collect(),
            successors: vec![],
        });
        block_id_counter += 1;
    }

    blocks
}

fn add_all_edges(graph: &mut Vec<Node>) {
    add_edge(NodeId::Entry, NodeId::BlockId(0), graph);

    for node in graph.clone().iter() {
        match node {
            Node::Entry { .. } | Node::Exit { .. } => continue,
            Node::Block {
                id, instructions, ..
            } => {
                let next_id;

                if *id == max_block_id(graph) {
                    next_id = NodeId::Exit;
                } else {
                    next_id = NodeId::BlockId(id_to_num(id) + 1);
                }

                let instr = instructions.last().unwrap();

                match instr {
                    IRInstruction::Ret(_) => add_edge(id.to_owned(), NodeId::Exit, graph),
                    IRInstruction::Jump(target) => {
                        let target_id = get_block_by_label(target, graph);
                        add_edge(id.to_owned(), target_id, graph);
                    }
                    IRInstruction::JumpIfZero { target, .. } => {
                        let target_id = get_block_by_label(target, graph);
                        add_edge(id.to_owned(), target_id, graph);
                        add_edge(id.to_owned(), next_id, graph);
                    }
                    IRInstruction::JumpIfNotZero { target, .. } => {
                        let target_id = get_block_by_label(target, graph);
                        add_edge(id.to_owned(), target_id, graph);
                        add_edge(id.to_owned(), next_id, graph);
                    }
                    _ => add_edge(id.to_owned(), next_id, graph),
                }
            }
        }
    }
}

pub fn add_edge(pred: NodeId, succ: NodeId, graph: &mut Vec<Node>) {
    fn add_id(nd_id: NodeId, id_list: &mut Vec<NodeId>) {
        if !id_list.contains(&nd_id) {
            id_list.push(nd_id);
        }
    }

    // Update the successors of the predecessor
    if let Some(Node::Block { id: _, successors, .. }) = graph
        .iter_mut()
        .find(|n| matches!(n, Node::Block { id, .. } if *id == pred))
    {
        add_id(succ.clone(), successors);
    }

    // Update the predecessors of the successor
    if let Some(Node::Block {
        id: _, predecessors, ..
    }) = graph
        .iter_mut()
        .find(|n| matches!(n, Node::Block { id, .. } if *id == succ))
    {
        add_id(pred.clone(), predecessors);
    }

    // Cover the entry and exit nodes
    if pred == NodeId::Entry {
        if let Some(Node::Entry { successors }) =
            graph.iter_mut().find(|n| matches!(n, Node::Entry { .. }))
        {
            add_id(succ.clone(), successors);
        }
    }

    if succ == NodeId::Exit {
        if let Some(Node::Exit { predecessors }) =
            graph.iter_mut().find(|n| matches!(n, Node::Exit { .. }))
        {
            add_id(pred.clone(), predecessors);
        }
    }
}

pub fn remove_edge(pred: NodeId, succ: NodeId, graph: &mut Vec<Node>) {
    fn remove_id(nd_id: NodeId, id_list: &mut Vec<NodeId>) {
        id_list.retain(|i| i.to_owned() != nd_id);
    }

    // Update the successors of the predecessor
    if let Some(Node::Block { id: _, successors, .. }) = graph
        .iter_mut()
        .find(|n| matches!(n, Node::Block { id, .. } if *id == pred))
    {
        remove_id(succ.clone(), successors);
    }

    // Update the predecessors of the successor
    if let Some(Node::Block {
        id: _, predecessors, ..
    }) = graph
        .iter_mut()
        .find(|n| matches!(n, Node::Block { id, .. } if *id == succ))
    {
        remove_id(pred.clone(), predecessors);
    }
}

fn id_to_num(id: &NodeId) -> usize {
    match id {
        NodeId::BlockId(num) => *num,
        _ => unreachable!(),
    }
}

fn get_block_by_label(label: &str, graph: &Vec<Node>) -> NodeId {
    graph.iter().find(|n| matches!(n, Node::Block { instructions, .. } if instructions.iter().any(|x| x == &IRInstruction::Label(label.to_owned()))))
        .map(|n| match n {
            Node::Block { id, .. } => id.to_owned(),
            _ => unreachable!(),
        })
        .unwrap()
}

fn max_block_id(graph: &Vec<Node>) -> NodeId {
    let mut max = 0;

    for node in graph {
        match node {
            Node::Block { id, .. } => {
                if let NodeId::BlockId(num) = id {
                    if *num > max {
                        max = *num;
                    }
                }
            }
            _ => continue,
        }
    }

    NodeId::BlockId(max)
}

pub fn instructions_to_cfg(instructions: &[IRInstruction]) -> Vec<Node> {
    let basic_blocks = create_basic_blocks(partition_into_basic_blocks(instructions));

    let mut graph = vec![Node::Entry { successors: vec![] }];
    graph.extend(basic_blocks);
    graph.push(Node::Exit {
        predecessors: vec![],
    });

    add_all_edges(&mut graph);

    graph
}

pub fn pretty_print_graph_as_graphviz(graph: &Vec<Node>) {
    use std::io::Write;

    // Print the graph in Graphviz format to file
    let mut dotfile = std::fs::File::create(format!("cfg.{}.dot", make_temporary())).unwrap();
    writeln!(dotfile, "digraph G {{").unwrap();

    for node in graph {
        match node {
            Node::Entry { successors } => {
                for succ in successors {
                    writeln!(dotfile, "  Entry -> {}", succ).unwrap();
                }
            }
            Node::Exit { predecessors } => {
                for pred in predecessors {
                    writeln!(dotfile, "  {} -> Exit", pred).unwrap();
                }
            }
            Node::Block {
                id,
                predecessors,
                successors,
                instructions,
            } => {
                // print instructions in the block as a table
                writeln!(dotfile, "  {} [shape=plaintext, label=<", id).unwrap();
                writeln!(
                    dotfile,
                    "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">"
                )
                .unwrap();
                writeln!(
                    dotfile,
                    "<tr><td colspan=\"2\">Block {}</td></tr>",
                    id_to_num(id)
                )
                .unwrap();
                for instr in instructions {
                    writeln!(
                        dotfile,
                        "<tr><td>{}</td><td>{:?}</td></tr>",
                        id_to_num(id),
                        instr
                    )
                    .unwrap();
                }
                writeln!(dotfile, "</table>>];").unwrap();

                for pred in predecessors {
                    writeln!(dotfile, "  {} -> {}", pred, id).unwrap();
                }

                for succ in successors {
                    writeln!(dotfile, "  {} -> {}", id, succ).unwrap();
                }
            }
        }
    }

    writeln!(dotfile, "}}").unwrap();
}

impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            NodeId::Entry => write!(f, "Entry"),
            NodeId::Exit => write!(f, "Exit"),
            NodeId::BlockId(num) => write!(f, "Block{}", num),
        }
    }
}
