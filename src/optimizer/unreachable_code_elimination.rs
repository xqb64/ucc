use crate::cfg::{self, BasicBlock, Instr, NodeId};
use std::collections::BTreeSet;
use std::fmt::Debug;

type NodeSet = BTreeSet<NodeId>;

pub fn unreachable_code_elimination<V: Clone + Debug, I: Debug + Instr + Clone>(
    cfg: &mut cfg::CFG<V, I>,
) -> &mut cfg::CFG<V, I> {
    remove_empty_blocks(eliminate_useless_labels(eliminate_useless_jumps(
        eliminate_unreachable_blocks(cfg),
    )))
}

pub fn eliminate_unreachable_blocks<V: Clone + Debug, I: Clone + Debug + Instr>(
    cfg: &mut cfg::CFG<V, I>,
) -> &mut cfg::CFG<V, I> {
    fn dfs<V: Clone + Debug, I: Clone + Debug + Instr>(
        cfg: &cfg::CFG<V, I>,
        explored: &mut NodeSet,
        node_id: NodeId,
    ) {
        if explored.contains(&node_id) {
            return;
        }

        explored.insert(node_id.clone());

        let succs = cfg.get_succs(&node_id);
        for succ in succs {
            dfs(cfg, explored, succ.clone());
        }
    }

    let mut reachable_block_ids = BTreeSet::new();
    dfs(cfg, &mut reachable_block_ids, NodeId::Entry);

    let mut edges_to_remove = vec![];
    let mut blocks_to_remove = vec![];

    let _: Vec<(usize, BasicBlock<V, I>)> = cfg
        .basic_blocks
        .iter()
        .filter(|(_, blk)| {
            if reachable_block_ids.contains(&blk.id) {
                true
            } else {
                for pred in &blk.preds {
                    edges_to_remove.push((pred.clone(), blk.id.clone()));
                }
                for succ in &blk.succs {
                    edges_to_remove.push((blk.id.clone(), succ.clone()));
                }
                blocks_to_remove.push(blk.id.clone());
                false
            }
        })
        .cloned()
        .collect();

    for block in blocks_to_remove {
        cfg.remove_block(block);
    }

    cfg
}

pub fn eliminate_useless_jumps<V: Clone + Debug, I: Clone + Debug + Instr>(
    cfg: &mut cfg::CFG<V, I>,
) -> &mut cfg::CFG<V, I> {
    fn drop_last<T>(vec: &mut Vec<T>) {
        vec.pop();
    }

    let updated_blocks: Vec<(usize, BasicBlock<V, I>)> = cfg
        .basic_blocks
        .iter()
        .enumerate()
        .map(|(idx, (n, blk))| {
            if idx == cfg.basic_blocks.len() - 1 {
                (*n, blk.clone())
            } else {
                match blk.instructions.last() {
                    Some((_, instr)) if instr.is_jump() => {
                        let (_, default_succ) = &cfg.basic_blocks[idx + 1];
                        if blk.succs.iter().all(|succ| succ == &default_succ.id) {
                            let mut new_blk = blk.clone();
                            drop_last(&mut new_blk.instructions);
                            (*n, new_blk)
                        } else {
                            (*n, blk.clone())
                        }
                    }
                    _ => (*n, blk.clone()),
                }
            }
        })
        .collect();

    cfg.basic_blocks = updated_blocks;

    cfg
}

pub fn eliminate_useless_labels<V: Clone + Debug, I: Clone + Debug + Instr>(
    cfg: &mut cfg::CFG<V, I>,
) -> &mut cfg::CFG<V, I> {
    let updated_blocks: Vec<(usize, BasicBlock<V, I>)> = cfg
        .basic_blocks
        .iter()
        .enumerate()
        .map(|(idx, (n, blk))| {
            if let Some((_, instr)) = blk.instructions.first() {
                if instr.is_label() {
                    let default_pred = if idx == 0 {
                        NodeId::Entry
                    } else {
                        cfg.basic_blocks[idx - 1].1.id.clone()
                    };

                    if blk.preds.iter().all(|pred| pred == &default_pred) {
                        let mut new_blk = blk.clone();
                        new_blk.instructions.remove(0);
                        return (*n, new_blk);
                    }
                }
            }
            (*n, blk.clone())
        })
        .collect();

    cfg.basic_blocks = updated_blocks;

    cfg
}

pub fn remove_empty_blocks<V: Clone + Debug, I: Clone + Debug + Instr>(
    cfg: &mut cfg::CFG<V, I>,
) -> &mut cfg::CFG<V, I> {
    let mut blocks_to_remove = Vec::new();

    let _: Vec<(usize, BasicBlock<V, I>)> = cfg
        .basic_blocks
        .iter()
        .filter(|(_, blk)| {
            if blk.instructions.is_empty() {
                blocks_to_remove.push(blk.id.clone());
                false
            } else {
                true
            }
        })
        .cloned()
        .collect();

    for block in blocks_to_remove {
        cfg.remove_block(block);
    }

    cfg
}
