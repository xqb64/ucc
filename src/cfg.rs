#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SimpleInstr {
    Label(String),
    ConditionalJump(String),
    UnconditionalJump(String),
    Return,
    Other,
}

pub trait Instr {
    fn simplify(&self) -> SimpleInstr;
    fn pp_instr(&self) -> String;  // Returns the instruction as a formatted string
    fn is_jump(&self) -> bool;
    fn is_label(&self) -> bool;
}

impl dyn Instr {
    fn is_jump(&self) -> bool {
        match self.simplify() {
            SimpleInstr::ConditionalJump(_)
            | SimpleInstr::UnconditionalJump(..) => true,
            _ => false,
        }
    }

    fn is_label(&self) -> bool {
        match self.simplify() {
            SimpleInstr::Label(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NodeId {
    Entry,
    Block(usize),  // Block ID is represented by an index
    Exit,
}

#[derive(Clone, Debug)]
pub struct BasicBlock<V, I>
where I: Clone + Instr, V: Clone {
    pub id: NodeId,
    pub instructions: Vec<(V, I)>,  // Tuple of value and instruction
    pub preds: Vec<NodeId>,          // Predecessor nodes
    pub succs: Vec<NodeId>,          // Successor nodes
    pub value: V,                    // Annotated value (e.g., live variables)
}

#[derive(Clone, Debug)]
pub struct CFG<V, I>
where I: Clone + Instr, V: Clone {
    pub basic_blocks: Vec<(usize, BasicBlock<V, I>)>,  // List of blocks indexed by ID
    pub entry_succs: Vec<NodeId>,                      // Successors of the entry node
    pub exit_preds: Vec<NodeId>,                       // Predecessors of the exit node
    pub debug_label: String,                           // Label for debugging
}

impl<V, I> CFG<V, I>
where I: Clone + Instr, V: Clone {
    pub fn get_succs(&self, node_id: &NodeId) -> Vec<NodeId> {
        match node_id {
            NodeId::Entry => self.entry_succs.clone(),
            NodeId::Block(n) => {
                let block = self.basic_blocks.iter().find(|(i, _)| *i == *n).unwrap();
                block.1.succs.clone()
            }
            NodeId::Exit => vec![],  // Exit node has no successors
        }
    }

    fn update_basic_block(&mut self, block_idx: usize, new_block: BasicBlock<V, I>) {
        for (idx, block) in &mut self.basic_blocks {
            if *idx == block_idx {
                *block = new_block;
                break;
            }
        }
    }

    pub fn add_edge(&mut self, pred: NodeId, succ: NodeId) {
        let add_if_missing = |node_list: &mut Vec<NodeId>, node_id: NodeId| {
            if !node_list.contains(&node_id) {
                node_list.push(node_id);
            }
        };
        
        // Add successor to predecessor
        match pred {
            NodeId::Entry => add_if_missing(&mut self.entry_succs, succ.clone()),
            NodeId::Block(n) => {
                let block = self.basic_blocks.iter_mut().find(|(i, _)| *i == n).unwrap();
                add_if_missing(&mut block.1.succs, succ.clone());
            }
            NodeId::Exit => panic!("Cannot add edges from the Exit node"),
        }

        // Add predecessor to successor
        match succ {
            NodeId::Block(n) => {
                let block = self.basic_blocks.iter_mut().find(|(i, _)| *i == n).unwrap();
                add_if_missing(&mut block.1.preds, pred);
            }
            NodeId::Exit => add_if_missing(&mut self.exit_preds, pred),
            NodeId::Entry => panic!("Cannot add edges to the Entry node"),
        }
    }

    fn partition_into_basic_blocks(instructions: Vec<I>) -> Vec<Vec<I>> {
        let mut finished_blocks = Vec::new();
        let mut current_block = Vec::new();

        for i in instructions {
            match i.simplify() {
                SimpleInstr::Label(_) => {
                    if !current_block.is_empty() {
                        finished_blocks.push(current_block);
                    }
                    current_block = vec![i];
                }
                SimpleInstr::ConditionalJump(_) | SimpleInstr::UnconditionalJump(_) | SimpleInstr::Return => {
                    current_block.push(i);
                    finished_blocks.push(current_block);
                    current_block = Vec::new();
                }
                SimpleInstr::Other => {
                    current_block.push(i);
                }
            }
        }

        if !current_block.is_empty() {
            finished_blocks.push(current_block);
        }

        finished_blocks
    }

    fn get_block_value(&self, blocknum: usize) -> &V {
        let block = self.basic_blocks.iter().find(|(i, _)| *i == blocknum).unwrap();
        &block.1.value
    }

    fn update_successors<F>(&mut self, node_id: &NodeId, f: F)
    where
        F: FnOnce(&mut Vec<NodeId>),
    {
        match node_id {
            NodeId::Entry => f(&mut self.entry_succs),
            NodeId::Block(n) => {
                let block = self.basic_blocks.iter_mut().find(|(i, _)| *i == *n).unwrap();
                f(&mut block.1.succs);
            }
            NodeId::Exit => panic!("Internal error: malformed CFG"),
        }
    }

    fn update_predecessors<F>(&mut self, node_id: &NodeId, f: F)
    where
        F: FnOnce(&mut Vec<NodeId>),
    {
        match node_id {
            NodeId::Exit => f(&mut self.exit_preds),
            NodeId::Block(n) => {
                let block = self.basic_blocks.iter_mut().find(|(i, _)| *i == *n).unwrap();
                f(&mut block.1.preds);
            }
            NodeId::Entry => panic!("Internal error: malformed CFG"),
        }
    }

    pub fn remove_edge(&mut self, pred: NodeId, succ: NodeId) {
        let remove_id = |vec: &mut Vec<NodeId>, id: &NodeId| {
            vec.retain(|x| x != id);
        };

        self.update_successors(&pred, |succs| remove_id(succs, &succ));
        self.update_predecessors(&succ, |preds| remove_id(preds, &pred));
    }

    fn add_all_edges(&mut self) {
        use std::collections::HashMap;

        // Create a map from labels to block IDs
        let mut label_map = HashMap::new();
        for (idx, block) in &self.basic_blocks {
            if let SimpleInstr::Label(lbl) = block.instructions[0].1.simplify() {
                label_map.insert(lbl.clone(), block.id.clone());
            }
        }

        // Add outgoing edges from each basic block
        for (id_num, block) in self.basic_blocks.clone() {
            let next_block = if id_num == self.basic_blocks.last().unwrap().0 {
                NodeId::Exit
            } else {
                NodeId::Block(id_num + 1)
            };

            let last_instr = &block.instructions.last().unwrap().1;

            match last_instr.simplify() {
                SimpleInstr::Return => self.add_edge(block.id.clone(), NodeId::Exit),
                SimpleInstr::UnconditionalJump(target) => {
                    let target_id = label_map.get(&target).unwrap();
                    self.add_edge(block.id.clone(), target_id.clone());
                }
                SimpleInstr::ConditionalJump(target) => {
                    let target_id = label_map.get(&target).unwrap();
                    self.add_edge(block.id.clone(), next_block.clone());
                    self.add_edge(block.id.clone(), target_id.clone());
                }
                _ => self.add_edge(block.id.clone(), next_block.clone()),
            }
        }

        // Finally, add edge from Entry to the first block
        self.add_edge(NodeId::Entry, NodeId::Block(0));
    }

    pub fn instructions_to_cfg(debug_label: String, instructions: Vec<I>) -> CFG<V, I>
    where
        V: Default,
    {
        let partitioned_blocks = Self::partition_into_basic_blocks(instructions);

        let basic_blocks = partitioned_blocks
            .into_iter()
            .enumerate()
            .map(|(idx, instrs)| {
                let instructions_with_ann = instrs.into_iter().map(|i| (V::default(), i)).collect();
                (
                    idx,
                    BasicBlock {
                        id: NodeId::Block(idx),
                        instructions: instructions_with_ann,
                        preds: vec![],
                        succs: vec![],
                        value: V::default(),
                    },
                )
            })
            .collect();

        let mut cfg = CFG {
            basic_blocks,
            entry_succs: vec![],
            exit_preds: vec![],
            debug_label,
        };

        cfg.add_all_edges();
        cfg
    }

    pub fn cfg_to_instructions(&self) -> Vec<I> 
    where
        I: Clone,
    {
        self.basic_blocks
            .iter()
            .flat_map(|(_, block)| block.instructions.iter().map(|(_, instr)| instr.clone()))
            .collect()
    }
}
