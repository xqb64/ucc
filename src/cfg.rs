use std::fmt::Debug;

use crate::ir::make_temporary;

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
    fn pp_instr(&self) -> String; // Returns the instruction as a formatted string
    fn is_jump(&self) -> bool;
    fn is_label(&self) -> bool;
}

impl dyn Instr {
    fn is_jump(&self) -> bool {
        match self.simplify() {
            SimpleInstr::ConditionalJump(_) | SimpleInstr::UnconditionalJump(..) => true,
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
    Block(usize), // Block ID is represented by an index
    Exit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BasicBlock<V, I>
where
    I: Clone + Instr,
    V: Clone + Debug,
{
    pub id: NodeId,
    pub instructions: Vec<(V, I)>, // Tuple of value and instruction
    pub preds: Vec<NodeId>,        // Predecessor nodes
    pub succs: Vec<NodeId>,        // Successor nodes
    pub value: V,                  // Annotated value (e.g., live variables)
}

#[derive(Clone, Debug)]
pub struct CFG<V, I>
where
    I: Clone + Instr,
    V: Clone + Debug,
{
    pub basic_blocks: Vec<(usize, BasicBlock<V, I>)>, // List of blocks indexed by ID
    pub entry_succs: Vec<NodeId>,                     // Successors of the entry node
    pub exit_preds: Vec<NodeId>,                      // Predecessors of the exit node
    pub debug_label: String,                          // Label for debugging
}

impl<V, I> CFG<V, I>
where
    I: Clone + Instr + Debug,
    V: Clone + Debug,
{
    pub fn get_block_by_id(&self, block_id: &NodeId) -> (usize, BasicBlock<V, I>) {
        match block_id {
            NodeId::Entry => panic!("Cannot get the Entry node"),
            NodeId::Block(_) => self
                .basic_blocks
                .iter()
                .find(|(_, blk)| &blk.id == block_id)
                .unwrap()
                .clone(),
            NodeId::Exit => panic!("Cannot get the Exit node"),
        }
    }

    pub fn get_succs(&self, node_id: &NodeId) -> Vec<NodeId> {
        match node_id {
            NodeId::Entry => self.entry_succs.clone(),
            NodeId::Block(n) => {
                let block = self.basic_blocks.iter().find(|(i, _)| *i == *n).unwrap();
                block.1.succs.clone()
            }
            NodeId::Exit => vec![], // Exit node has no successors
        }
    }

    pub fn get_preds(&self, node_id: &NodeId) -> Vec<NodeId> {
        match node_id {
            NodeId::Exit => self.exit_preds.clone(),
            NodeId::Block(n) => {
                let block = self.basic_blocks.iter().find(|(i, _)| *i == *n).unwrap();
                block.1.preds.clone()
            }
            NodeId::Entry => vec![], // Entry node has no predecessors
        }
    }

    pub fn strip_annotations(self) -> CFG<(), I> {
        CFG {
            basic_blocks: self
                .basic_blocks
                .into_iter()
                .map(|(idx, block)| {
                    (
                        idx,
                        BasicBlock {
                            id: block.id,
                            instructions: block
                                .instructions
                                .into_iter()
                                .map(|(_, instr)| ((), instr))
                                .collect(),
                            preds: block.preds,
                            succs: block.succs,
                            value: (),
                        },
                    )
                })
                .collect(),
            entry_succs: self.entry_succs,
            exit_preds: self.exit_preds,
            debug_label: self.debug_label,
        }
    }

    pub fn initialize_annotation<T: Clone + Debug>(self, dummy_val: T) -> CFG<T, I> {
        // Initialize a single instruction with the provided dummy_val
        let initialize_instruction = |(_, instr): (V, I)| -> (T, I) { (dummy_val.clone(), instr) };
    
        // Initialize a block by updating its instructions and value
        let initialize_block =
            |(idx, block): (usize, BasicBlock<V, I>)| -> (usize, BasicBlock<T, I>) {
                (
                    idx,
                    BasicBlock {
                        id: block.id,
                        // Apply the dummy_val to each instruction
                        instructions: block
                            .instructions
                            .into_iter()
                            .map(initialize_instruction)
                            .collect(),
                        preds: block.preds,
                        succs: block.succs,
                        // Set the block's value to dummy_val
                        value: dummy_val.clone(),
                    },
                )
            };
    
        // Apply the changes to each block in the CFG
        CFG {
            basic_blocks: self
                .basic_blocks
                .into_iter()
                .map(initialize_block)
                .collect(),
            entry_succs: self.entry_succs,
            exit_preds: self.exit_preds,
            debug_label: self.debug_label,
        }
    }
    
    pub fn update_basic_block(&mut self, block_idx: usize, new_block: BasicBlock<V, I>) {
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
                SimpleInstr::ConditionalJump(_)
                | SimpleInstr::UnconditionalJump(_)
                | SimpleInstr::Return => {
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

    pub fn get_block_value(&self, blocknum: usize) -> &V {
        let block = self
            .basic_blocks
            .iter()
            .find(|(i, _)| *i == blocknum)
            .unwrap();
        &block.1.value
    }

    fn update_successors<F>(&mut self, node_id: &NodeId, f: F)
    where
        F: FnOnce(&mut Vec<NodeId>),
    {
        match node_id {
            NodeId::Entry => f(&mut self.entry_succs),
            NodeId::Block(_) => {
                let block = self
                    .basic_blocks
                    .iter_mut()
                    .find(|(_, blk)| blk.id == *node_id)
                    .unwrap();
                f(&mut block.1.succs);
            }
            NodeId::Exit => panic!("Internal error: malformed CFG"),
        }
    }

    pub fn remove_block(&mut self, block_id: NodeId) {
        // First, collect the successors and predecessors that need to be updated
        let mut successors_to_update = Vec::new();
        let mut predecessors_to_update = Vec::new();
    
        // Collect successors of the block
        self.update_successors(&block_id, |succs| {
            successors_to_update.extend(succs.clone()); // Clone the successors
            succs.clear(); // Clear all successors of the block
        });
    
        // Collect predecessors of the block
        self.update_predecessors(&block_id, |preds| {
            predecessors_to_update.extend(preds.clone()); // Clone the predecessors
            preds.clear(); // Clear all predecessors of the block
        });
    
        // Now update all the predecessors by removing the block from their successor lists
        for succ in &successors_to_update {
            self.update_predecessors(succ, |preds| {
                preds.retain(|x| x != &block_id); // Remove block from predecessors
            });
        }
    
        // Now update all the successors by removing the block from their predecessor lists
        for pred in &predecessors_to_update {
            self.update_successors(pred, |succs| {
                succs.retain(|x| x != &block_id); // Remove block from successors
            });
        }
    
        // Reconnect the block's predecessors to its successors, skipping the deleted block
        for pred in predecessors_to_update {
            for succ in &successors_to_update {
                if pred != block_id && succ != &block_id {
                    self.add_edge(pred.clone(), succ.clone());
                }
            }
        }
    
        // Finally, remove the block from the basic blocks
        self.basic_blocks.retain(|(_, blk)| blk.id != block_id);
    
        // If the entry node is affected, reconnect it to the first available block
        if block_id == NodeId::Block(0) {
            if let Some(block) = self.basic_blocks.first_mut() {
                self.entry_succs.clear(); // Clear entry successors first
                self.entry_succs.push(block.1.id.clone());
                block.1.preds.push(NodeId::Entry);
            }
        }
    }
    
    fn update_predecessors<F>(&mut self, node_id: &NodeId, f: F)
    where
        F: FnOnce(&mut Vec<NodeId>),
    {
        match node_id {
            NodeId::Exit => f(&mut self.exit_preds),
            NodeId::Block(n) => {
                let block = self
                    .basic_blocks
                    .iter_mut()
                    .find(|(i, _)| *i == *n)
                    .unwrap();
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
        for (_, block) in &self.basic_blocks {
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

    pub fn print_as_graphviz(&self) {
        use std::fs::File;
        use std::io::Write;

        let tmp = make_temporary();
        let path = format!("cfg.{}.dot", tmp);

        let mut file = File::create(path.clone()).unwrap();
        writeln!(file, "digraph {{").unwrap();

        // Graph general settings
        writeln!(file, "  labeljust=l;").unwrap();
        writeln!(file, "  node[shape=\"box\"];").unwrap();
        writeln!(file, "  Entry[label=\"ENTRY\"];").unwrap();
        writeln!(file, "  Exit[label=\"EXIT\"];").unwrap();

        // Function to extract node ids
        fn extract_id(node: &NodeId) -> String {
            match node {
                NodeId::Entry => "Entry".to_string(),
                NodeId::Block(n) => format!("Block{}", n),
                NodeId::Exit => "Exit".to_string(),
            }
        }

        // Print the basic blocks
        for (idx, block) in &self.basic_blocks {
            writeln!(file, "  Block{}[label=<", idx).unwrap();
            writeln!(
                file,
                "    <table border=\"0\" cellborder=\"1\" cellspacing=\"0\">"
            )
            .unwrap();
            writeln!(
                file,
                "      <tr><td colspan=\"2\"><b>Block {}</b></td></tr>",
                idx
            )
            .unwrap();

            // Print the instructions and value annotations
            for (val, instr) in &block.instructions {
                writeln!(
                    file,
                    "      <tr><td align=\"left\">{}</td><td align=\"left\">{:?}</td></tr>",
                    instr.pp_instr(),
                    val
                )
                .unwrap();
            }

            // Add any block annotations
            writeln!(
                file,
                "      <tr><td colspan=\"2\">{:?}</td></tr>",
                block.value
            )
            .unwrap();

            writeln!(file, "    </table>").unwrap();
            writeln!(file, "  >];").unwrap();
        }

        // Print the entry edges
        for succ in &self.entry_succs {
            writeln!(file, "  Entry -> {};", extract_id(succ)).unwrap();
        }

        // Print the block edges
        for (idx, block) in &self.basic_blocks {
            for succ in &block.succs {
                writeln!(file, "  Block{} -> {};", idx, extract_id(succ)).unwrap();
            }
        }

        // Print the exit edges
        for pred in &self.exit_preds {
            writeln!(file, "  {} -> Exit;", extract_id(pred)).unwrap();
        }

        writeln!(file, "}}").unwrap();

        println!("running dot command");
        // Convert the dot file to png
        std::process::Command::new("dot")
            .arg("-Tpng")
            .arg(path)
            .arg("-o")
            .arg(format!("cfg.{}.png", tmp))
            .output()
            .expect("Failed to execute dot command");
    }
}
