/// Creates a code generation friendly IR with evaluation order and pipeline evaluation info
///
/// Inorder to gather such information we do structural analysis of RTLolaMIR graph
/// Evaluation layer provided in RTLolaMIR assumes the alternate evaluation of event-based & periodic streams which is no longer valid here
/// Also if a node depends on data via past offsets, RTLolaMIR thinks the node can be evaluated right in the beginning as all the dependencies are in the past
/// However, that assumes we are not doing evaluation in a pipeline fashion where the past values might not have been fully evaluated yet
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
/// Here, e can't be evaluated before b & d as the past value might not have reached b & d yet
/// For a correct evaluation we could just eval e after b & d
/// However, depending on the offset it depends on, it can potentially be evaluated earlier in pipeline
/// Potentially saving us a cycle and providing faster output of e
/// Check refine_eval_order function for more info
///
/// For these reasons, we cannot directly use the evaluation layer provided in RTLolaMIR as it is
/// So the evaluation order and pipeline info is identified by the structural dependency analysis of the RTLolaMIR graph
/// However, the evaluation layer in RTLolaMIR is still used to guess the initial set of root nodes in the graph (see extract_roots function)
use std::{
    cmp::Ordering, collections::{HashMap, HashSet}, hash::Hash
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
    fn from_access(access: &(StreamReference, Vec<(Origin, StreamAccessKind)>)) -> Node {
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
                    children.push(Node::from_access(child));
                }
            }
            Node::OutputStream(x) => {
                for child in &mir.outputs[x.clone()].accessed_by {
                    children.push(Node::from_access(child));
                }
            }
            Node::SlidingWindow(x) => {
                children.push(Node::from_stream(&mir.sliding_windows[x.clone()].caller));
            }
        }
        children
    }
    fn get_parents(&self, mir: &RtLolaMir) -> Vec<Node> {
        let mut parents: Vec<Node> = Vec::new();
        match self {
            Node::OutputStream(x) => {
                for parent in &mir.outputs[x.clone()].accesses {
                    parents.push(Node::from_access(parent));
                }
            }
            Node::SlidingWindow(x) => {
                parents.push(Node::from_stream(&mir.sliding_windows[x.clone()].target));
            }
            _ => {}
        }
        parents
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
    // For each node that consumes data with offset
    // Check if source is evaluated after
    // If the source is evaluated after time (t) with t > offset
    // Then the node can't be evaluated in pipelined fashion
    // Due to data dependency all the children and parents of the node can't be pipelined either
    unimplemented!()
}

pub fn get_eval_order(mir: &RtLolaMir) -> Vec<Vec<Node>> {
    // Starting from the roots explore nodes in the level-order
    //
    // For example:
    // input x : Int32
    // output a := x + 1
    // output b := a + 1
    // output c := x + b
    // 
    // Here the levels will be [x], [a, c], [b]
    // Here in the level [a, c] we could be evaluated after [x]
    // However as c is reachable from a, so we should remove c from the 2nd level
    // Giving us the order: [[x], [a], [b], [c]]
    // 
    // However, with a loop this simple idea won't work
    // 
    // For example: 
    // input x : Int32
    // output a := x + 1
    // output b := x + d.offset(by: -1).defaults(to: 0)
    // output c := a + b
    // output d := c + 1
    // 
    // Here the 2nd level would be [a, b] 
    // However b is reachable from 'a' via the loop
    // If we ignore b in the first level we would get the order [[a], [c], [d], [b]]
    // Which is wrong as c must be evaluated only after we have both a & b
    //
    // Note that the loop can only exist along the -ve offset
    // So we could remove the -ve offset to avoid the loop in the graph
    // As the source node for a -ve offset must be in the past, removing the edge doesn't create a dependency issue
    // However, not all -ve edges create a loop
    //
    // For example:
    // input x : Int
    // output a := x + 1
    // output b := a + 1
    // output c := b.offset(by: -1).defaults(to: 0) 
    //
    // Here if we remove -ve offset of b-c, c would be disconnected 
    // As c has pacing @x, it would mean we can evaluated c as soon as we have x
    // Giving the order [[x], [a,c], [b]]
    // However, in a pipeline execution this would be wrong
    // t1 t2 t3 t4 ..
    // x1 x2 x3 x4 ..
    // .. a1 a2 a3 ..
    // .. .. b1 b2 ..
    // As you can see, at eval cycle 2
    // the past value of b i.e b1 is only ready by time t4
    // so the evaluation order [[x], [a,c], [b]] is wrong
    // as we can't evaluate c along with a because the past value of b is not ready yet
    // However, depending on the offset we can evaluate it earlier
    // See eval order refinement algorithm for more details i.e function refine_eval_order
    //
    // So, to identify the correct eval order we should only remove those offsets that results in a cycle
    // After that we can order the nodes in a level-order
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
        cur_level = order_nodes_by_reachability(mir, next_level.clone(), &visited);
        next_level = vec![];
    }
    order
}



fn order_nodes_by_reachability(mir: &RtLolaMir, nodes: Vec<Node>, frozen: &HashSet<Node>) -> Vec<Node> {
    let no_duplicate: Vec<Node> = nodes.clone().into_iter().collect::<HashSet<_>>().into_iter().collect();
    let mut ordered = no_duplicate;
    ordered.sort_by(|a, b| {
        let unreachables = remove_reachable_roots(mir, vec![a.clone(), b.clone()], frozen);
        if unreachables.contains(a) {Ordering::Less} else {Ordering::Greater}
    });
    ordered
}

pub fn refine_eval_order(mir: &RtLolaMir, eval_order: Vec<Vec<Node>>) -> Vec<Vec<Node>> {
    // When the node only depends on the past values of other nodes
    // It can potentially be evaluated earlier
    // For example:
    // output x @1Hz := x.offset(by: -1).defaults(to: 0) + 1
    // output a := x + 1
    // output b := x + 1
    // output c := a + 1
    // output d := c + 1
    // output e := b.offset(by: -1).defaults(to: 0) + d.offset(by: -1).defaults(to: 0)
    //
    // Here the eval order would be x -> a, b -> c -> d -> e
    // However, as e depends only on the past (-1 offset) values of d & e
    // Without affecting the pipeline, it can be evaluated earlier like: x -> a,b -> c -> d,e
    // If e depends on d by -3 offset then we could evalaute it even earlier
    // i.e x -> a, b, e -> c -> d
    // which can be found by checking the level of b & d in the eval order
    // b = level 1, d = level 3
    // In pipeline evaluation at each cycle we calculate a new value for a step
    // At level 3, we are about to calcuate new value of d which means we just have the -1 offset value ready there
    // So if we want -3 offset value of d it would be already availabe at level - (offset - 1)
    // We can choose the minimum level >= 0 which is valid for both b & d
    // And that's the earliest level where e can be evaluated in pipeline
    //
    // When we evaluate a node earlier, some of it's children could also be evaluated earlier
    // Also, we need to consider that it can have children only depending via offsets
    // So, this offset based analysis should be run enough times to catch those opportunities
    // Also the chilif it had children dpending only on it, they could
    // So, after adapting this node the children could be impoved fsame analysis must be also run on the children
    let mut eval_order = eval_order;

    // Running analysis multiple times to accomodate the same "offset only deps" based refinement for children
    for _ in 0..mir.outputs.len() {
        for out in &mir.outputs {
            let mut all_past_deps = true;
            let mut deps: Vec<(Node, usize)> = Vec::new();
            for child in &out.accesses {
                match child.1.first().unwrap().1 {
                    StreamAccessKind::Offset(off) => {
                        deps.push((
                            Node::from_stream(&child.0),
                            match off {
                                Offset::Past(x) => x as usize,
                                _ => unreachable!(),
                            },
                        ));
                    }
                    _ => {
                        all_past_deps = false;
                    }
                }
            }
            if all_past_deps {
                let new_level = deps
                    .into_iter()
                    .map(|(nd, offset)| {
                        let level = find_level(&nd, &eval_order);
                        let earliest_posible = if level > (offset - 1) {
                            level - (offset - 1)
                        } else {
                            0
                        };
                        earliest_posible
                    })
                    .max()
                    .unwrap();
                let node = Node::OutputStream(out.reference.out_ix());
                let old_level = find_level(&node, &eval_order);
                if new_level < old_level {
                    eval_order = patch_eval_order(node, old_level, new_level, eval_order);
                }
            }
        }
    }

    // Possible to evaluate nodes earlier after "offset only" refinement
    let all_nodes: Vec<Node> = eval_order.iter().flatten().map(|x| x.clone()).collect();
    for _ in 0..all_nodes.len() {
        for node in &all_nodes {
            let old_level = find_level(node, &eval_order);
            let new_level = node
                .get_parents(mir)
                .into_iter()
                .map(|parent| find_level(&parent, &eval_order) + 1)
                .max()
                .unwrap();
            if new_level < old_level {
                eval_order = patch_eval_order(node.clone(), old_level, new_level, eval_order);
            }
        }
    }
    eval_order.into_iter().filter(|x| !x.is_empty()).collect()
}

fn patch_eval_order(
    node: Node,
    old_level: usize,
    new_level: usize,
    eval_order: Vec<Vec<Node>>,
) -> Vec<Vec<Node>> {
    let mut eval_order = eval_order;
    eval_order[old_level].retain(|nd| *nd != node);
    eval_order[new_level].push(node);
    eval_order
}

fn find_level(node: &Node, eval_order: &Vec<Vec<Node>>) -> usize {
    for (i, nodes) in eval_order.iter().enumerate() {
        if nodes.contains(node) {
            return i;
        }
    }
    unreachable!()
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
