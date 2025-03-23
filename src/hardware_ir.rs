/// Creates a code generation friendly IR with evaluation order and pipeline evaluation info
///
/// Inorder to gather such information we do structural analysis of RTLolaMIR graph
/// Evaluation layer provided in RTLolaMIR assumes the alternate evaluation of event-based & periodic streams which is no longer valid here
/// Also if a node depends on data via past offsets, RTLolaMIR thinks the node can be evaluated right in the beginning as all the dependencies are in the past
/// However, that assumes we are not doing evaluation in a pipeline fashion. 
/// When the offset exists along a long path from the input, then even the past data might not be ready already
/// 
/// For example:
/// ```
/// output x @1Hz := x.offset(by: -1).defaults(to: 0) + 1
/// output a := x + 1
/// output b := x + 1
/// output c := a + 1
/// output d := c + 1
/// output e := b.offset(by: -1).defaults(to: 0) + d.offset(by: -1).defaults(to: 0)
/// ```
/// 
/// Here, e must be evaluated after b & d in-pipeline fashion
/// However, If the offset is sufficiently large we don't have to wait for b & d
/// 
/// For these reasons, we cannot directly use the evaluation layer provided in RTLolaMIR as it is
/// So the evaluation order and pipeline info is identified by the structural dependency analysis of the RTLolaMIR graph
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use rtlola_frontend::mir::{
    Offset, OutputStream, RtLolaMir, Stream, StreamAccessKind, StreamReference,
};

use crate::utils;

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord)]
pub enum Node {
    InputStream(usize),
    OutputStream(usize),
    SlidingWindow(usize),
}

#[derive(PartialEq, Clone)]
struct HardwareIR {
    mir: RtLolaMir,
    evaluation_order: Vec<Vec<Node>>,
}

impl HardwareIR {
    fn new(mir: RtLolaMir) -> Self {
        HardwareIR {
            mir: mir,
            evaluation_order: vec![],
        }
    }
}

fn mark_non_pipeline() {
    // Cycle in RTLola can only exist along the offset edge
    // For each node that consumes data from offset
    // Check the distance (d) along the eval tree to reach such source
    // If we can reach then the cycle exists
    // If the distance > offset value, then the data transfer takes too long
    // Which means the current node can't be evaluated in a pipeline fashion
    // Due to the data dependency all of the children & parents of a node that can't be pipelined can't be pipelined as well
    unimplemented!()
}

fn build_eval_tree(mir: &RtLolaMir, roots: &Vec<StreamReference>) -> (Vec<StreamReference>, Vec<Vec<StreamReference>>) {
    // Starting from the roots explore nodes in level-order
    // Naive BFS won't work as there could be multiple ways to reach the same descendent node
    // In such cases, we must choose the longest way to reach a descendent node due to the data dependency
    // For this, we order nodes s.t the ones that can reach the others under consideration must be evaluated first
    // Time complexity: O(v) * O(v * e) = O(v^2 * e)

    let mut eval_tree: Vec<Vec<StreamReference>> = Vec::new();
    for _ in 0..(mir.inputs.len() + mir.outputs.len()) {
        eval_tree.push(Vec::new());
    }

    let roots = extract_roots(mir);
    for root in &roots {
        let children = match root {
            StreamReference::In(x) => mir.inputs[x.clone()].accessed_by(),
            StreamReference::Out(x) => mir.outputs[x.clone()].accessed_by()
        };
        for child in children {
            // self loop
            if root.is_output() && child.0.out_ix() == root.out_ix() {
                continue;
            }
            // TODO:   -------
        }
    }
    // TODO: -------- 

    let mut cur_level: Vec<usize> = Vec::new();
    let mut next_level: Vec<usize> = Vec::new();

    // TODO: -------- 
    
    (roots, eval_tree)
}

pub fn extract_roots(mir: &RtLolaMir) -> Vec<StreamReference> {
    // There can be a sub-graph which generates periodic signals without consuming the inputs
    // Hence we can't reach all the nodes just by traversing from the inputs
    // Such disjoint periodic graph can be a cycle which means just looking at the structure of the graph we can't identify the root
    // However, RTLola frontend performs HIR layer analysis according to the language semantics
    // We can choose the nodes with the smallest evaluation layer in the disjoing graph as the roots

    let mut roots: Vec<usize> = Vec::new();
    for root in &mir.inputs {
        for nd in &root.accessed_by {
            roots.push(nd.0.out_ix());
        }
    }
    let reached = traverse(mir, roots);

    let mut retval: Vec<StreamReference> = Vec::new();
    for inpt in &mir.inputs {
        retval.push(inpt.reference.clone());
    }
    let mut disjoint: Vec<&OutputStream> = reached
        .iter()
        .enumerate()
        .filter(|(i, &x)| !x)
        .map(|(i, _)| &mir.outputs[i])
        .collect();
    disjoint.sort_by_key(|&x| x.eval_layer());
    if !disjoint.is_empty() {
        let min_layer = disjoint.first().unwrap().eval_layer();
        for nd in disjoint {
            if nd.eval_layer() != min_layer {
                break;
            }
            retval.push(nd.reference.clone());
        }
    }
    retval
}

fn traverse(mir: &RtLolaMir, roots: Vec<usize>) -> Vec<bool> {
    let mut visited = vec![false; mir.outputs.len()];
    let mut q: Vec<usize> = roots;

    while !q.is_empty() {
        let nd = q.pop().unwrap();
        visited[nd] = true;
        for child in &mir.outputs[nd].accessed_by {
            if !visited[child.0.out_ix()] {
                q.push(child.0.out_ix());
            }
        }
    }
    visited
}

// pub fn node_tree(mir: RtLolaMir) -> Vec<Vec<Node>> {
//     let mut orders = node_orders_from_sliding_windows(&mir);
//     let ouput_orders = node_orders_from_outputs(&mir);
//     orders.extend(ouput_orders);
//     merge_nodes_into_tree(orders)
// }

// fn merge_nodes_into_tree(orders: Vec<Vec<Node>>) -> Vec<Vec<Node>> {
//     let mut set = HashSet::new();
//     for order in orders.clone() {
//         for node in order {
//             set.insert(node);
//         }
//     }
//     let mut keys: Vec<Node> = set.into_iter().collect();
//     keys.sort_by(|a, b| a.cmp(&b));
//     let mut map = HashMap::new();
//     let mut reverse_map = HashMap::new();
//     for (i, item) in keys.into_iter().enumerate() {
//         map.insert(item.clone(), i);
//         reverse_map.insert(i, item);
//     }
//     let orders: Vec<Vec<usize>> = orders
//         .into_iter()
//         .map(|v| {
//             v.into_iter()
//                 .map(|a| map.get(&a).unwrap().clone())
//                 .collect()
//         })
//         .collect();
//     let merged_orders = utils::merge_orders(orders);
//     let merged_orders: Vec<Vec<Node>> = merged_orders
//         .into_iter()
//         .map(|v| {
//             v.into_iter()
//                 .map(|i| reverse_map.get(&i).unwrap().clone())
//                 .collect()
//         })
//         .collect();
//     merged_orders
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn merge_nodes_into_tree_works() {
//         // from sliding window analysis
//         let mut orders = vec![
//             vec![
//                 Node::OutputStream(1),
//                 Node::SlidingWindow(0),
//                 Node::OutputStream(2),
//             ],
//             vec![
//                 Node::OutputStream(2),
//                 Node::SlidingWindow(2),
//                 Node::OutputStream(4),
//             ],
//             vec![
//                 Node::OutputStream(0),
//                 Node::SlidingWindow(1),
//                 Node::OutputStream(3),
//             ],
//         ];
//         // from outputs analysis
//         let output_orders = vec![
//             vec![
//                 Node::Offset(OffsetNode {
//                     from: Box::new(Node::InputStream(0)),
//                     by: 1,
//                 }),
//                 Node::OutputStream(0),
//             ],
//             vec![Node::OutputStream(0), Node::OutputStream(1)],
//             vec![Node::SlidingWindow(0), Node::OutputStream(2)],
//             vec![Node::SlidingWindow(1), Node::OutputStream(3)],
//             vec![Node::SlidingWindow(2), Node::OutputStream(4)],
//         ];
//         orders.extend(output_orders);

//         let expected = vec![
//             vec![Node::Offset(OffsetNode {
//                 from: Box::new(Node::InputStream(0)),
//                 by: 1,
//             })],
//             vec![Node::OutputStream(0)],
//             vec![Node::OutputStream(1), Node::SlidingWindow(1)],
//             vec![Node::OutputStream(3), Node::SlidingWindow(0)],
//             vec![Node::OutputStream(2)],
//             vec![Node::SlidingWindow(2)],
//             vec![Node::OutputStream(4)],
//         ];

//         let actual = merge_nodes_into_tree(orders);
//         assert_eq!(actual, expected);
//     }
// }
