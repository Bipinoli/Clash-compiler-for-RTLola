/// Creates a code generation friendly IR with evaluation order and pipeline evaluation info
///
/// Inorder to gather such information we do structural analysis of RTLolaMIR graph
/// Evaluation layer provided in RTLolaMIR assumes the alternate evaluation of event-based & periodic streams which is no longer valid here
/// Also if a node depends on data via past offsets, RTLolaMIR thinks the node can be evaluated right in the beginning as all the dependencies are in the past
/// However, that assumes we are not doing evaluation in a pipeline fashion.
/// When the offset exists along a long path from the input, then even the past data might not be ready already
///
/// For example:
/// output x @1Hz := x.offset(by: -1).defaults(to: 0) + 1
/// output a := x + 1
/// output b := x + 1
/// output c := a + 1
/// output d := c + 1
/// output e := b.offset(by: -1).defaults(to: 0) + d.offset(by: -1).defaults(to: 0)
///
/// Here, e must be evaluated after b & d in-pipeline fashion
/// However, If the offset is sufficiently large we don't have to wait for b & d
///
/// For these reasons, we cannot directly use the evaluation layer provided in RTLolaMIR as it is
/// So the evaluation order and pipeline info is identified by the structural dependency analysis of the RTLolaMIR graph
/// However, the evaluation layer in RTLolaMIR is still used to guess the initial set of root nodes in the graph (see extract_roots function)
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use rtlola_frontend::mir::{

    Offset, Origin, OutputStream, RtLolaMir, Stream, StreamAccessKind, StreamReference,
};

use crate::utils;

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord)]
pub enum Node {
    InputStream(usize),
    OutputStream(usize),
    SlidingWindow(usize),
}
impl Node {
    fn from_stream(stream_ref: &StreamReference) -> Node {
        match stream_ref {
            StreamReference::In(x) => Node::InputStream(x.clone()),
            StreamReference::Out(x) => Node::OutputStream(x.clone()),
        }
    }
    fn from_accessed_by(access: &(StreamReference, Vec<(Origin, StreamAccessKind)>)) -> Node {
        match access.1.first().unwrap().1 {
            StreamAccessKind::SlidingWindow(x) => Node::SlidingWindow(x.idx()),
            _ => Node::OutputStream(access.0.out_ix()),
        }
    }
    fn get_children(&self, mir: &RtLolaMir) -> Vec<Node> {
        let mut children: Vec<Node> = Vec::new();
        match self {
            Node::InputStream(x) => {
                for child in &mir.inputs[x.clone()].accessed_by {
                    children.push(Node::from_accessed_by(child));
                }
            }
            Node::OutputStream(x) => {
                for child in &mir.outputs[x.clone()].accessed_by {
                    children.push(Node::from_accessed_by(child));
                }
            }
            Node::SlidingWindow(x) => {
                let child = Node::OutputStream(mir.sliding_windows[x.clone()].caller.out_ix());
                children.push(child);
            }
        }
        children
    }
    fn out_ix(&self) -> usize {
        match self {
            Node::OutputStream(x) => x.clone(),
            _ => unreachable!(),
        }
    }
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

pub fn get_eval_order(mir: &RtLolaMir) -> Vec<Vec<Node>> {
    // Starting from the roots explore nodes in level-order
    // Naive BFS won't work as there could be multiple ways to reach the same descendent node
    // In such cases, we must choose the longest way to reach a descendent node due to the data dependency

    let mut order: Vec<Vec<Node>> = Vec::new();

    let mut next_level: Vec<Node> = Vec::new();
    let mut cur_level: Vec<Node> = extract_roots(mir);

    let mut visited: HashSet<Node> = HashSet::new();
    while !cur_level.is_empty() {
        order.push(cur_level.clone());
        cur_level.iter().for_each(|nd| {
            visited.insert(nd.clone());
        });
        for node in &cur_level {
            for child in node.get_children(mir) {
                if !visited.contains(&child) {
                    next_level.push(child);
                }
            }
        }
        // check reachability without going through the already visited nodes
        cur_level = remove_reachable_roots(mir, next_level.clone(), &visited);
        next_level = vec![];
    }
    order
}

pub fn extract_roots(mir: &RtLolaMir) -> Vec<Node> {
    // There can be a sub-graph which generates periodic signals without consuming the inputs
    // Hence we can't reach all the nodes just by traversing from the inputs
    // To identify roots in such disjointed graph we can start by estimating the ones with the lowest eval layer in RTLolaMIR
    //
    // For example in:
    // output a @1Hz := c.offset(by: -1).defaults(to: 0) + 1
    // output b := a + 1
    // output c := b + 1
    //
    // a will have the lowest layer therefore an estimate for a root
    // This is because, when we don't have a pipeline evaluation any past offset is guaranteed to have been evaluated
    // Hence the layer can be identifed by ignoring the offsets
    //
    // However offset can also exist without a cycle
    //
    // For example in:
    // output x @1Hz := x.offset(by: -1).defaults(to: 0) + 1
    // output a := x + 1
    // output b := x + 1
    // output c := a + 1
    // output d := c + 1
    // output e := b.offset(by: -1).defaults(to: 0) + d.offset(by: -1).defaults(to: 0)
    //
    // x & e have the lowest evaluation layers as the layer is calucated by RTLola by ignoring the offset edges
    // however e is reachable from x so only 'x' should be a root
    // So we need to do reachablility analysis on the initial estimate to find actual roots
    //
    // There can also be a case when it is not clear which node should be a root
    // For example:
    // output a @1Hz := c.offset(by: -1).defaults(to: 0) + 1
    // output b := a.offset(by: -1).defaults(to: 0) + 1
    // output c := b.offset(by: -1).defaults(to: 0) + 1
    //
    // Here all nodes are reachable starting from any one and they have the same eval layers (calcualted by removing offset edges)
    // Hence any arbitrary node can be chosen as a root

    let mut roots: Vec<Node> = Vec::new();
    for inpt in &mir.inputs {
        roots.push(Node::InputStream(inpt.reference.in_ix()));
    }
    let reached = traverse(mir, roots.clone(), &HashSet::new());

    let mut disjoint: Vec<Node> = Vec::new();
    for i in 0..mir.outputs.len() {
        let nd = Node::OutputStream(i);
        if !reached.contains(&nd) {
            disjoint.push(nd);
        }
    }
    disjoint.sort_by_key(|nd| match nd {
        Node::SlidingWindow(_) => usize::MAX,
        Node::OutputStream(x) => mir.outputs[x.clone()].eval_layer().inner(),
        Node::InputStream(_) => unreachable!(),
    });
    if !disjoint.is_empty() {
        let first = disjoint.first().unwrap().clone();
        let estimated_roots: Vec<Node> = disjoint
            .into_iter()
            .take_while(|nd| match nd {
                Node::OutputStream(x) => {
                    let nd_layer = mir.outputs[x.clone()].eval_layer();
                    let first_layer = mir.outputs[first.out_ix()].eval_layer();
                    nd_layer == first_layer
                }
                _ => false,
            })
            .collect();
        for root in remove_reachable_roots(mir, estimated_roots, &HashSet::new()) {
            roots.push(root);
        }
    }
    roots
}

fn remove_reachable_roots(mir: &RtLolaMir, roots: Vec<Node>, frozen: &HashSet<Node>) -> Vec<Node> {
    let mut unreachable: HashSet<Node> = HashSet::from_iter(roots.clone());
    for node in roots {
        if !unreachable.contains(&node) {
            continue;
        }
        traverse(mir, vec![node.clone()], frozen)
            .into_iter()
            .for_each(|nd| {
                if nd != node {
                    unreachable.remove(&nd);
                }
            });
    }
    unreachable.into_iter().collect()
}

fn traverse(mir: &RtLolaMir, roots: Vec<Node>, frozen: &HashSet<Node>) -> HashSet<Node> {
    let mut reached: HashSet<Node> = HashSet::new();
    let mut stk: Vec<Node> = roots;

    while !stk.is_empty() {
        let nd = stk.pop().unwrap();
        reached.insert(nd.clone());
        for child in nd.get_children(mir) {
            if !reached.contains(&child) & !frozen.contains(&child) {
                stk.push(child);
            }
        }
    }
    reached
}
