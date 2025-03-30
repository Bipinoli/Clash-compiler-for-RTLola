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
    cmp::{self, Ordering}, collections::{HashMap, HashSet}, hash::Hash, usize, vec
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
            _ => Node::from_stream(&access.0),
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

    fn get_non_offset_children(&self, mir: &RtLolaMir) -> Vec<Node> {
        let mut children: Vec<Node> = Vec::new();
        match self {
            Node::InputStream(x) => {
                for child in &mir.inputs[x.clone()].accessed_by {
                    match child.1.first().unwrap().1 {
                        StreamAccessKind::Offset(_) => {}
                        _ => {
                            children.push(Node::from_access(child));
                        }
                    }
                }
            }
            Node::OutputStream(x) => {
                for child in &mir.outputs[x.clone()].accessed_by {
                    match child.1.first().unwrap().1 {
                        StreamAccessKind::Offset(_) => {}
                        _ => {
                            children.push(Node::from_access(child));
                        }
                    }
                }
            }
            Node::SlidingWindow(x) => {
                children.push(Node::from_stream(&mir.sliding_windows[x.clone()].caller));
            }
        }
        children
    }

    // fn get_real_children(&self, mir: &RtLolaMir) -> Vec<Node> {
    //     // children except for the ones that make loop with -ve offset
    //     let mut real_children: Vec<Node> = Vec::new();
    //     let children = self.get_children(mir);
    //     for child in children {
    //         let is_past_offset_child = {
    //             match child {
    //                 Node::OutputStream(x) => {
    //                     let past_offsets = &mir.outputs[x.clone()]
    //                         .accesses
    //                         .iter()
    //                         .filter(|access| {
    //                             let parent_node = Node::from_stream(&access.0);
    //                             parent_node == *self
    //                         })
    //                         .map(|access| {
    //                             let access_kind = access.1.first().unwrap().1;
    //                             match access_kind {
    //                                 StreamAccessKind::Offset(off) => match off {
    //                                     Offset::Past(_) => true,
    //                                     _ => false,
    //                                 },
    //                                 _ => false,
    //                             }
    //                         })
    //                         .filter(|x| *x)
    //                         .collect::<Vec<_>>();
    //                     !past_offsets.is_empty()
    //                 }
    //                 Node::SlidingWindow(_) => false,
    //                 Node::InputStream(_) => unreachable!(),
    //             }
    //         };
    //         let is_real_child = if is_past_offset_child {
    //             let reached = traverse(mir, vec![child.clone()], &HashSet::new());
    //             !reached.contains(self)
    //         } else {
    //             true
    //         };
    //         if is_real_child {
    //             real_children.push(child);
    //         }
    //     }
    //     let children_str = real_children.iter().map(|nd| nd.prettify(mir)).collect::<Vec<_>>().join(", ");
    //     real_children
    // }

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

    fn get_offset_parents(&self, mir: &RtLolaMir) -> Vec<(Node, usize)> {
        let mut parents: Vec<(Node, usize)> = Vec::new();
        match self {
            Node::OutputStream(x) => {
                for parent in &mir.outputs[x.clone()].accesses {
                    match parent.1.first().unwrap().1 {
                        StreamAccessKind::Offset(off) => match off {
                            Offset::Past(x) => {
                                parents.push((Node::from_access(parent), x.clone() as usize));
                            }
                            _ => {
                                unreachable!()
                            }
                        },
                        _ => {}
                    }
                }
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

    pub fn prettify(&self, mir: &RtLolaMir) -> String {
        match self {
            Node::InputStream(x) => mir.inputs[x.clone()].name.clone(),
            Node::OutputStream(x) => mir.outputs[x.clone()].name.clone(),
            Node::SlidingWindow(x) => {
                let caller = {
                    mir.outputs[mir.sliding_windows[x.clone()].caller.out_ix()]
                        .name
                        .clone()
                };
                let target = {
                    let target = mir.sliding_windows[x.clone()].target;
                    if target.is_output() {
                        mir.outputs[target.out_ix()].name.clone()
                    } else {
                        mir.inputs[target.in_ix()].name.clone()
                    }
                };
                format!("sw({},{})", target, caller)
            }
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

pub fn visualize_pipeline(
    eval_order: &Vec<Vec<Node>>,
    pipeline_wait: usize,
    time_steps: usize,
    mir: &RtLolaMir,
) {
    let orders: Vec<String> = eval_order
        .iter()
        .map(|order| {
            order
                .iter()
                .map(|nd| nd.prettify(mir))
                .collect::<Vec<String>>()
                .join(",")
        })
        .collect();
    let max_len = orders.iter().map(|x| x.len()).max().unwrap();
    for (i, x) in orders.iter().enumerate() {
        let mut line: Vec<String> = Vec::new();
        let mut shift_cnt = 0;
        for j in 0..time_steps {
            let padded_str = if j >= i && shift_cnt == 0 {
                shift_cnt = pipeline_wait;
                format!("{:width$}", x.as_str(), width = max_len)
            } else {
                if j >= i {
                    shift_cnt -= 1;
                }
                format!("{:width$}", "", width = max_len)
            };
            line.push(padded_str);
        }
        let line = line.join(" | ");
        println!("{}", line);
        println!("{}", "-".repeat(line.len()));
    }
}

fn analyse_for_pipelining(eval_order: &Vec<Vec<Node>>, mir: &RtLolaMir) {
    unimplemented!()
}

fn window_size(
    node: &Node,
    child: &Node,
    pipeline_wait: usize,
    offset: usize,
    eval_order: &Vec<Vec<Node>>,
) -> usize {
    let dist = level_distance(node, child, eval_order);
    if dist <= 0 {
        let dist = (-dist) as f32;
        ((offset as f32) + 1.0 - (dist / (pipeline_wait as f32 + 1.0)).ceil()) as usize
    } else {
        ((dist as f32) / (pipeline_wait as f32 + 1.0)).ceil() as usize
    }
}

fn pipeline_wait(node: &Node, parent: &Node, offset: usize, eval_order: &Vec<Vec<Node>>, mir: &RtLolaMir) -> usize {
    let propagation_time = level_distance(parent, node, eval_order);
    let value_avail_time = propagation_time + 1; // 1 cycle for the eval
    // we must wait until 
    // value_avail_time <= (wait + 1) * offset
    let wait = if value_avail_time < 0 {
        0
    } else {
        (((value_avail_time as f32)/(offset as f32)).ceil() - 1.0) as usize
    }; 
    // println!("pipeline wait {} - {} = {}, offset = {}", parent.prettify(mir), node.prettify(mir), wait, offset);
    wait
}

fn level_distance(a: &Node, b: &Node, eval_order: &Vec<Vec<Node>>) -> i32 {
    let level_a = find_level(a, eval_order) as i32;
    let level_b = find_level(b, eval_order) as i32;
    level_a - level_b
}

fn find_necessary_pipeline_wait(eval_order: &Vec<Vec<Node>>, mir: &RtLolaMir) -> usize {
    let all_nodes: Vec<Node> = eval_order.iter().flatten().map(|x| x.clone()).collect();
    let max_shift = all_nodes
        .into_iter()
        .map(|node| {
            node.get_offset_parents(mir)
                .into_iter()
                .map(|(parent, offset)| pipeline_wait(&node, &parent, offset, eval_order, mir))
                .max()
        })
        .max();
    max_shift.unwrap_or(Some(0)).unwrap()
}

pub fn find_eval_order(mir: &RtLolaMir) -> usize {
    // First find the eval orders ignoring the offsets
    // This can give us disjointed sub-graphs
    // If we don't want to pipeline then we can run them in parallel
    // As in non-pipeline evaluation, the next eval cycle will only run after completely finishing earlier
    // This gives us correct evaluation as all past values (offsets) will be available
    // However, it might still be possible to pipeline the evaluation
    // For this we could check the dependency by offsets and check the maximum pipeline_wait necessary
    // The amount of necesary pipeline_wait also depends on when we want to start evaluation of the disjointed graphs
    // For this we can try all the possible combinations and choose the one which requires the minimum pipeline_wait
    //
    // For example:
    // input x: Int
    // output a := x + 1
    // output b := a + 1
    // output c := b + 1
    // output d := c.offset(by: -1).defaults(to: 0)
    //
    // After removing offsets the isolated graphs with eval orders are [x, a, b, c] and [d]
    // We could try all combinations of eval orders i.e:
    // [(x, d), a, b, c], [x, (a, d), b, c], [x, a, (b, d), c], [x, a, b, (c, d)], [x, a, b, c, d]
    // Among them the eval order [x, a, b, (c, d)] & [x, a, b, c, d] will give the least pipeline_wait necessary i.e 0
    // However [x, a, b, (c, d)] is more desirable as the total eval layers are smaller and we get to evaluate d as early as possible
    // When we look deeper, it makes sense as when evaluating c we will already have the past value of c availabe
    // Whereas if we had tried to evaluate d along with b then we would need to wait one cycle until the c gets evaluated
    // It is possible to avoid checking all possbile combinations using this observations
    // However for the sake of simplicity we can simpley check all the combinations
    // Given the time complexity of O(n ^ (k + 1)) is acceptable
    // where n = max length of eval order ~ number of nodes
    //       k = number of disjointed sub-graphs
    //      here we have n choice to start k eval-orders and each pipeline_wait calculation takes O(n) hence k + 1 in the exponent

    let eval_orders = find_disjoint_eval_orders(mir);
    let n = eval_orders.first().unwrap().len();
    let all_starts = gen_all_combinations(n, eval_orders.len() - 1);
    let all_combinations: Vec<Vec<Vec<Node>>> = all_starts.into_iter().map(|start| {
        merge_eval_orders(&eval_orders, start)
    }).collect::<HashSet<_>>().into_iter().collect();

    let mut best_pipeline_wait = usize::MAX;
    all_combinations.into_iter().for_each(|eval_order| {
        println!("\n{}", prettify_eval_order(&eval_order, mir));
        let pipeline_wait = find_necessary_pipeline_wait(&eval_order, mir);
        visualize_pipeline(&eval_order, pipeline_wait, 10, mir);
        best_pipeline_wait = cmp::min(best_pipeline_wait, pipeline_wait);
    });
    best_pipeline_wait
}


fn gen_all_combinations(n: usize, k: usize) -> Vec<Vec<usize>> {
    let mut result = Vec::new();
    gen_combination(n, k, &mut Vec::new(), &mut result);
    result
}

fn gen_combination(n: usize, k: usize, cur: &mut Vec<usize>, result: &mut Vec<Vec<usize>>) {
    if cur.len() == k {
        result.push(cur.clone());
        return;
    }
    for i in 0..=(2*n) {
        cur.push(i);
        gen_combination(n, k, cur, result);
        cur.pop();
    }
}

fn merge_eval_orders(eval_orders: &Vec<Vec<Vec<Node>>>, starts: Vec<usize>) -> Vec<Vec<Node>> {
    assert!(eval_orders.len() == starts.len() + 1);
    let eval_orders: Vec<Vec<Vec<Node>>> = eval_orders.iter().enumerate().map(|(i, orders)| {
        if i > 0 {
            let mut extended: Vec<Vec<Node>> = vec![Vec::new(); starts[i-1]];
            extended.extend(orders.clone());
            extended
        } else {
            orders.clone()
        }
    }).collect();
    let max_len = eval_orders.iter().map(|eval_order| eval_order.len()).max().unwrap();
    let extended_eval_orders: Vec<Vec<Vec<Node>>> = eval_orders.into_iter().map(|eval_order| {
        let to_pad = max_len - eval_order.len();
        let padding: Vec<Vec<Node>> = vec![Vec::new(); to_pad];
        let mut eval_order = eval_order;
        eval_order.extend(padding);
        eval_order
    }).collect();
    let mut merged: Vec<Vec<Node>> = Vec::new();
    for i in 0..max_len {
        let level: Vec<Node> = extended_eval_orders.iter().map(|eval_order| {
            eval_order[i].clone()
        }).fold(Vec::new(), |acc, item| {
            let mut acc = acc;
            acc.extend(item);
            acc
        });
        merged.push(level);
    }
    merged.into_iter().filter(|level| !level.is_empty()).collect()
}

/// returns disjoint eval orders ordered_by their length in desc order
pub fn find_disjoint_eval_orders(mir: &RtLolaMir) -> Vec<Vec<Vec<Node>>> {
    let mut orders: Vec<Vec<Vec<Node>>> = get_roots(mir)
        .into_iter()
        .map(|roots| dag_eval_order(roots, mir))
        .collect();
    orders.sort_by(|a, b| b.len().cmp(&a.len()));
    orders
}

/// get roots of the graph after ignoring offset edges
/// roots are grouped together if they belog to the same connected sub-graph
pub fn get_roots(mir: &RtLolaMir) -> Vec<Vec<Node>> {
    let inputs: Vec<Node> = mir
        .inputs
        .iter()
        .map(|inpt| Node::from_stream(&inpt.reference))
        .collect();
    let mut roots = inputs.clone();
    let unreachable_from_inputs: Vec<Node> = {
        let reached = traverse(mir, inputs);
        mir.outputs
            .iter()
            .map(|output| Node::from_stream(&output.reference))
            .filter(|nd| !reached.contains(nd))
            .collect()
    };
    let other_roots = remove_reachable_roots(unreachable_from_inputs, mir);
    roots.extend(other_roots);

    let reachables: Vec<HashSet<Node>> = roots
        .iter()
        .map(|nd| traverse(mir, vec![nd.clone()]))
        .collect();
    let mut groups: Vec<usize> = roots
        .iter()
        .enumerate()
        .into_iter()
        .map(|(i, _)| i.clone())
        .collect();
    for i in 0..roots.len() {
        for j in (i + 1)..roots.len() {
            if reachables[i].intersection(&reachables[j]).count() > 0 {
                let min_group = cmp::min(groups[i], groups[j]);
                groups[i] = min_group;
                groups[j] = min_group;
            }
        }
    }
    let mut root_groups: Vec<Vec<Node>> = roots.iter().map(|_| vec![]).collect();
    for (i, rt) in roots.into_iter().enumerate() {
        root_groups[groups[i]].push(rt);
    }
    root_groups.into_iter().filter(|v| !v.is_empty()).collect()
}

fn traverse(mir: &RtLolaMir, roots: Vec<Node>) -> HashSet<Node> {
    let mut reached: HashSet<Node> = HashSet::new();
    let mut stk: Vec<Node> = roots;

    while !stk.is_empty() {
        let nd = stk.pop().unwrap();
        reached.insert(nd.clone());
        for child in nd.get_non_offset_children(mir) {
            if !reached.contains(&child) {
                stk.push(child);
            }
        }
    }
    reached
}

// evaluation order from a DAG (directed acyclic graph)
pub fn dag_eval_order(roots: Vec<Node>, mir: &RtLolaMir) -> Vec<Vec<Node>> {
    let mut order: Vec<Vec<Node>> = Vec::new();

    let mut cur_level: Vec<Node> = roots;
    let mut next_level: Vec<Node> = Vec::new();

    while !cur_level.is_empty() {
        order.push(cur_level.clone());
        for node in &cur_level {
            for child in node.get_non_offset_children(mir) {
                next_level.push(child);
            }
        }
        cur_level = remove_reachable_roots(next_level, mir);
        next_level = vec![];
    }
    order
}

fn remove_reachable_roots(roots: Vec<Node>, mir: &RtLolaMir) -> Vec<Node> {
    let all_reachables: HashSet<Node> = roots
        .iter()
        .map(|nd| {
            traverse(mir, vec![nd.clone()])
                .into_iter()
                .filter(|r| *r != *nd)
                .collect::<HashSet<_>>()
        })
        .collect::<Vec<HashSet<_>>>()
        .iter()
        .fold(HashSet::new(), |acc, item| {
            let mut all_reached = acc;
            all_reached.extend(item.clone());
            all_reached
        });
    roots
        .into_iter()
        .filter(|nd| !all_reachables.contains(nd))
        .collect::<HashSet<_>>()
        .into_iter()
        .collect()
}

fn find_level(node: &Node, eval_order: &Vec<Vec<Node>>) -> usize {
    for (i, nodes) in eval_order.iter().enumerate() {
        if nodes.contains(node) {
            return i;
        }
    }
    unreachable!()
}

// pub fn refine_eval_order(mir: &RtLolaMir, eval_order: Vec<Vec<Node>>) -> Vec<Vec<Node>> {
//     // When the node only depends on the past values of other nodes
//     // It can potentially be evaluated earlier
//     // For example:
//     // output x @1Hz := x.offset(by: -1).defaults(to: 0) + 1
//     // output a := x + 1
//     // output b := x + 1
//     // output c := a + 1
//     // output d := c + 1
//     // output e := b.offset(by: -1).defaults(to: 0) + d.offset(by: -1).defaults(to: 0)
//     //
//     // Here the eval order would be x -> a, b -> c -> d -> e
//     // However, as e depends only on the past (-1 offset) values of d & e
//     // Without affecting the pipeline, it can be evaluated earlier like: x -> a,b -> c -> d,e
//     // If e depends on d by -3 offset then we could evalaute it even earlier
//     // i.e x -> a, b, e -> c -> d
//     // which can be found by checking the level of b & d in the eval order
//     // b = level 1, d = level 3
//     // In pipeline evaluation at each cycle we calculate a new value for a step
//     // At level 3, we are about to calcuate new value of d which means we just have the -1 offset value ready there
//     // So if we want -3 offset value of d it would be already availabe at level - (offset - 1)
//     // We can choose the minimum level >= 0 which is valid for both b & d
//     // And that's the earliest level where e can be evaluated in pipeline
//     //
//     // When we evaluate a node earlier, some of it's children could also be evaluated earlier
//     // Also, we need to consider that it can have children only depending via offsets
//     // So, this offset based analysis should be run enough times to catch those opportunities
//     // Also the chilif it had children dpending only on it, they could
//     // So, after adapting this node the children could be impoved fsame analysis must be also run on the children
//     let mut eval_order = eval_order;

//     // Running analysis multiple times to accomodate the same "offset only deps" based refinement for children
//     for _ in 0..mir.outputs.len() {
//         for out in &mir.outputs {
//             let mut all_past_deps = true;
//             let mut deps: Vec<(Node, usize)> = Vec::new();
//             for child in &out.accesses {
//                 match child.1.first().unwrap().1 {
//                     StreamAccessKind::Offset(off) => {
//                         deps.push((
//                             Node::from_stream(&child.0),
//                             match off {
//                                 Offset::Past(x) => x as usize,
//                                 _ => unreachable!(),
//                             },
//                         ));
//                     }
//                     _ => {
//                         all_past_deps = false;
//                     }
//                 }
//             }
//             if all_past_deps {
//                 let new_level = deps
//                     .into_iter()
//                     .map(|(nd, offset)| {
//                         let level = find_level(&nd, &eval_order);
//                         let earliest_posible = if level > (offset - 1) {
//                             level - (offset - 1)
//                         } else {
//                             0
//                         };
//                         earliest_posible
//                     })
//                     .max()
//                     .unwrap();
//                 let node = Node::OutputStream(out.reference.out_ix());
//                 let old_level = find_level(&node, &eval_order);
//                 if new_level < old_level {
//                     eval_order = patch_eval_order(node, old_level, new_level, eval_order);
//                 }
//             }
//         }
//     }

//     // Possible to evaluate nodes earlier after "offset only" refinement
//     let all_nodes: Vec<Node> = eval_order.iter().flatten().map(|x| x.clone()).collect();
//     for _ in 0..all_nodes.len() {
//         for node in &all_nodes {
//             let old_level = find_level(node, &eval_order);
//             let new_level = node
//                 .get_parents(mir)
//                 .into_iter()
//                 .map(|parent| find_level(&parent, &eval_order) + 1)
//                 .max()
//                 .unwrap_or(old_level.clone());
//             if new_level < old_level {
//                 eval_order = patch_eval_order(node.clone(), old_level, new_level, eval_order);
//             }
//         }
//     }
//     eval_order.into_iter().filter(|x| !x.is_empty()).collect()
// }

// fn patch_eval_order(
//     node: Node,
//     old_level: usize,
//     new_level: usize,
//     eval_order: Vec<Vec<Node>>,
// ) -> Vec<Vec<Node>> {
//     let mut eval_order = eval_order;
//     eval_order[old_level].retain(|nd| *nd != node);
//     eval_order[new_level].push(node);
//     eval_order
// }

// pub fn extract_roots(mir: &RtLolaMir) -> Vec<Node> {
//     // There can be a sub-graph which generates periodic signals without consuming the inputs
//     // Hence we can't reach all the nodes just by traversing from the inputs
//     // To identify roots in such disjointed graph we can start by estimating the ones with the lowest eval layer in RTLolaMIR
//     //
//     // For example in:
//     // output a @1Hz := c.offset(by: -1).defaults(to: 0) + 1
//     // output b := a + 1
//     // output c := b + 1
//     //
//     // a will have the lowest layer therefore an estimate for a root
//     // This is because, when we don't have a pipeline evaluation any past offset is guaranteed to have been evaluated
//     // Hence the layer can be identifed by ignoring the offsets
//     //
//     // However offset can also exist without a cycle
//     //
//     // For example in:
//     // output x @1Hz := x.offset(by: -1).defaults(to: 0) + 1
//     // output a := x + 1
//     // output b := x + 1
//     // output c := a + 1
//     // output d := c + 1
//     // output e := b.offset(by: -1).defaults(to: 0) + d.offset(by: -1).defaults(to: 0)
//     //
//     // x & e have the lowest evaluation layers as the layer is calucated by RTLola by ignoring the offset edges
//     // however e is reachable from x so only 'x' should be a root
//     // So we need to do reachablility analysis on the initial estimate to find actual roots
//     //
//     // There can also be a case when it is not clear which node should be a root
//     // For example:
//     // output a @1Hz := c.offset(by: -1).defaults(to: 0) + 1
//     // output b := a.offset(by: -1).defaults(to: 0) + 1
//     // output c := b.offset(by: -1).defaults(to: 0) + 1
//     //
//     // Here all nodes are reachable starting from any one and they have the same eval layers (calcualted by removing offset edges)
//     // Hence any arbitrary node can be chosen as a root

//     let mut roots: Vec<Node> = Vec::new();
//     for inpt in &mir.inputs {
//         roots.push(Node::InputStream(inpt.reference.in_ix()));
//     }
//     let reached = traverse(mir, roots.clone(), &HashSet::new());

//     let mut disjoint: Vec<Node> = Vec::new();
//     for i in 0..mir.outputs.len() {
//         let nd = Node::OutputStream(i);
//         if !reached.contains(&nd) {
//             disjoint.push(nd);
//         }
//     }
//     disjoint.sort_by_key(|nd| match nd {
//         Node::SlidingWindow(_) => usize::MAX,
//         Node::OutputStream(x) => mir.outputs[x.clone()].eval_layer().inner(),
//         Node::InputStream(_) => unreachable!(),
//     });
//     if !disjoint.is_empty() {
//         let first = disjoint.first().unwrap().clone();
//         let estimated_roots: Vec<Node> = disjoint
//             .into_iter()
//             .take_while(|nd| match nd {
//                 Node::OutputStream(x) => {
//                     let nd_layer = mir.outputs[x.clone()].eval_layer();
//                     let first_layer = mir.outputs[first.out_ix()].eval_layer();
//                     nd_layer == first_layer
//                 }
//                 _ => false,
//             })
//             .collect();
//         for root in remove_reachable_roots(mir, estimated_roots, &HashSet::new()) {
//             roots.push(root);
//         }
//     }
//     roots
// }

// fn traverse_real_children(
//     mir: &RtLolaMir,
//     roots: Vec<Node>,
//     frozen: &HashSet<Node>,
// ) -> HashSet<Node> {
//     let mut reached: HashSet<Node> = HashSet::new();
//     let mut stk: Vec<Node> = roots;

//     while !stk.is_empty() {
//         let nd = stk.pop().unwrap();
//         reached.insert(nd.clone());
//         for child in nd.get_real_children(mir) {
//             if !reached.contains(&child) & !frozen.contains(&child) {
//                 stk.push(child);
//             }
//         }
//     }
//     reached
// }

// fn traverse(mir: &RtLolaMir, roots: Vec<Node>, frozen: &HashSet<Node>) -> HashSet<Node> {
//     let mut reached: HashSet<Node> = HashSet::new();
//     let mut stk: Vec<Node> = roots;

//     while !stk.is_empty() {
//         let nd = stk.pop().unwrap();
//         reached.insert(nd.clone());
//         for child in nd.get_children(mir) {
//             if !reached.contains(&child) & !frozen.contains(&child) {
//                 stk.push(child);
//             }
//         }
//     }
//     reached
// }

pub fn prettify_eval_order(eval_order: &Vec<Vec<Node>>, mir: &RtLolaMir) -> String {
    eval_order
        .iter()
        .map(|order| {
            order
                .iter()
                .map(|nd| nd.prettify(mir))
                .collect::<Vec<_>>()
                .join(", ")
        })
        .collect::<Vec<_>>()
        .join("\n")
}
