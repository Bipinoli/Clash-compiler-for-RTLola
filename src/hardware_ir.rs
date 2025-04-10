/// Creates a code generation friendly IR with evaluation order and pipeline evaluation info
///
/// Inorder to gather such information we do structural analysis of RTLolaMIR graph
/// Evaluation layer provided in RTLolaMIR assumes the alternate evaluation of event-based & periodic streams which is no longer valid here
/// Also if a node depends on data via past offsets, RTLolaMIR thinks the node can be evaluated right in the beginning as all the dependencies are in the past
/// However, that assumes we are not doing evaluation in a pipeline fashion where the past values might not have been fully evaluated yet
use serde::Serialize;
use std::{
    cmp::{self},
    collections::{HashMap, HashSet},
    hash::Hash,
    usize, vec,
};

use rtlola_frontend::mir::{Offset, Origin, RtLolaMir, StreamAccessKind, StreamReference};

static DISPLAY_ALL_COMBINATIONS: bool = false;

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord, Serialize)]
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

    fn get_children(&self, mir: &RtLolaMir) -> Vec<(Node, usize)> {
        let mut children: Vec<(Node, usize)> = Vec::new();
        match self {
            Node::InputStream(x) => {
                for child in &mir.inputs[x.clone()].accessed_by {
                    let offset = match child.1.first().unwrap().1 {
                        StreamAccessKind::Offset(off) => match off {
                            Offset::Past(x) => x as usize,
                            _ => unreachable!(),
                        },
                        _ => 0,
                    };
                    children.push((Node::from_access(child), offset));
                }
            }
            Node::OutputStream(x) => {
                for child in &mir.outputs[x.clone()].accessed_by {
                    let offset = match child.1.first().unwrap().1 {
                        StreamAccessKind::Offset(off) => match off {
                            Offset::Past(x) => x as usize,
                            _ => unreachable!(),
                        },
                        _ => 0,
                    };
                    children.push((Node::from_access(child), offset));
                }
            }
            Node::SlidingWindow(x) => {
                children.push((Node::from_stream(&mir.sliding_windows[x.clone()].caller), 0));
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

#[derive(PartialEq, Clone, Debug, Serialize)]
pub struct HardwareIR {
    pub mir: RtLolaMir,
    pub evaluation_order: Vec<Vec<Node>>,
    pub pipeline_wait: usize,
    pub required_memory: HashMap<Node, usize>,
    pub spec: String,
    pub spec_name: String,
}

impl HardwareIR {
    pub fn new(mir: RtLolaMir, spec: String, spec_name: String) -> Self {
        let eval_order = find_eval_order(&mir, false);
        let pipeline_wait = calculate_necessary_pipeline_wait(&eval_order, &mir);
        let required_memory = calculate_required_memory(&eval_order, &mir);
        HardwareIR {
            evaluation_order: eval_order,
            mir,
            pipeline_wait,
            required_memory,
            spec,
            spec_name,
        }
    }
}

pub fn display_analysis(mir: &RtLolaMir) {
    let eval_order = find_eval_order(mir, DISPLAY_ALL_COMBINATIONS);
    println!("\n------- The best pipeline --------");
    println!("{}\n", prettify_eval_order(&eval_order, mir).join("\n"));
    println!(
        "{}\n",
        prettify_required_memory(&eval_order, mir).join("\n")
    );
    let pipeline_wait = calculate_necessary_pipeline_wait(&eval_order, mir);
    println!(
        "{}\n",
        visualize_pipeline(&eval_order, pipeline_wait, 10, mir).join("\n")
    );
}

pub fn visualize_pipeline(
    eval_order: &Vec<Vec<Node>>,
    pipeline_wait: usize,
    time_steps: usize,
    mir: &RtLolaMir,
) -> Vec<String> {
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
    let mut visual: Vec<String> = Vec::new();
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
        visual.push(format!("{}", line));
        visual.push(format!("{}", "-".repeat(line.len())));
    }
    visual
}

fn calculate_required_memory(eval_order: &Vec<Vec<Node>>, mir: &RtLolaMir) -> HashMap<Node, usize> {
    let mut needed_memory: HashMap<Node, usize> = HashMap::new();
    let pipeline_wait = calculate_necessary_pipeline_wait(eval_order, mir);
    let all_nodes: Vec<Node> = eval_order.iter().flatten().map(|x| x.clone()).collect();
    all_nodes.into_iter().for_each(|node| {
        let window = node
            .get_children(mir)
            .into_iter()
            .map(|(child, offset)| {
                window_size(&node, &child, pipeline_wait, offset, eval_order, mir)
            })
            .max()
            .unwrap_or(1);
        needed_memory.insert(node, window);
    });
    needed_memory
}

pub fn prettify_required_memory(eval_order: &Vec<Vec<Node>>, mir: &RtLolaMir) -> Vec<String> {
    let pipeline_wait = calculate_necessary_pipeline_wait(eval_order, mir);
    let all_nodes: Vec<Node> = eval_order.iter().flatten().map(|x| x.clone()).collect();
    let mut retval: Vec<String> = Vec::new();
    all_nodes.into_iter().for_each(|node| {
        let window = node
            .get_children(mir)
            .into_iter()
            .map(|(child, offset)| {
                window_size(&node, &child, pipeline_wait, offset, eval_order, mir)
            })
            .max()
            .unwrap_or(1);
        retval.push(format!("window {} = {}", node.prettify(mir), window));
    });
    retval
}

fn window_size(
    node: &Node,
    child: &Node,
    pipeline_wait: usize,
    offset: usize,
    eval_order: &Vec<Vec<Node>>,
    _mir: &RtLolaMir,
) -> usize {
    let propagation_time = level_distance(node, child, eval_order);
    let window = if propagation_time < 0 {
        let propagation_time = -propagation_time;
        let time_left_after_propagation = (pipeline_wait + 1) * offset + propagation_time as usize;
        let num_of_evals =
            ((time_left_after_propagation as f32) / (pipeline_wait as f32 + 1.0)).ceil();
        num_of_evals as usize
    } else {
        let time_left_after_propagation = (pipeline_wait + 1) * offset - propagation_time as usize;
        let num_of_evals =
            ((time_left_after_propagation as f32) / (pipeline_wait as f32 + 1.0)).ceil();
        num_of_evals as usize
    };
    // println!("window {} due to child {} = {}, offset = {}, prop_time = {}", node.prettify(mir), child.prettify(mir), window, offset, propagation_time);
    window
}

fn calculate_pipeline_wait(
    node: &Node,
    parent: &Node,
    offset: usize,
    eval_order: &Vec<Vec<Node>>,
    _mir: &RtLolaMir,
) -> usize {
    let propagation_time = level_distance(parent, node, eval_order);
    let value_avail_time = propagation_time + 1; // 1 cycle for the eval
                                                 // we must wait until: value_avail_time <= (wait + 1) * offset
    let wait = if value_avail_time < 0 {
        0
    } else {
        (((value_avail_time as f32) / (offset as f32)).ceil() - 1.0) as usize
    };
    // println!("pipeline wait {} - {} = {}, offset = {}", parent.prettify(mir), node.prettify(mir), wait, offset);
    wait
}

fn level_distance(a: &Node, b: &Node, eval_order: &Vec<Vec<Node>>) -> i32 {
    let level_a = find_level(a, eval_order) as i32;
    let level_b = find_level(b, eval_order) as i32;
    level_a - level_b
}

fn calculate_necessary_pipeline_wait(eval_order: &Vec<Vec<Node>>, mir: &RtLolaMir) -> usize {
    let all_nodes: Vec<Node> = eval_order.iter().flatten().map(|x| x.clone()).collect();
    let max_shift = all_nodes
        .into_iter()
        .map(|node| {
            node.get_offset_parents(mir)
                .into_iter()
                .map(|(parent, offset)| {
                    calculate_pipeline_wait(&node, &parent, offset, eval_order, mir)
                })
                .max()
                .unwrap_or(0)
        })
        .max();
    max_shift.unwrap_or(0)
}

fn find_eval_order(mir: &RtLolaMir, display_all_combinations: bool) -> Vec<Vec<Node>> {
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
    let all_combinations: Vec<Vec<Vec<Node>>> = all_starts
        .into_iter()
        .map(|start| merge_eval_orders(&eval_orders, start))
        .collect::<HashSet<_>>()
        .into_iter()
        .collect();

    if display_all_combinations {
        all_combinations.iter().for_each(|eval_order| {
            println!();
            println!("{}\n", prettify_eval_order(&eval_order, mir).join("\n"));
            println!(
                "{}\n",
                prettify_required_memory(&eval_order, mir).join("\n")
            );
            let pipeline_wait = calculate_necessary_pipeline_wait(&eval_order, mir);
            visualize_pipeline(&eval_order, pipeline_wait, 10, mir);
        });
    }
    all_combinations
        .into_iter()
        .reduce(|best, item| choose_better_eval_order(best, item, mir))
        .unwrap()
}

fn choose_better_eval_order(
    eval_order1: Vec<Vec<Node>>,
    eval_order2: Vec<Vec<Node>>,
    mir: &RtLolaMir,
) -> Vec<Vec<Node>> {
    let pipeline_wait1 = calculate_necessary_pipeline_wait(&eval_order1, mir);
    let pipeline_wait2 = calculate_necessary_pipeline_wait(&eval_order2, mir);
    if pipeline_wait1 < pipeline_wait2 {
        return eval_order1;
    }
    if pipeline_wait2 < pipeline_wait1 {
        return eval_order2;
    }
    if eval_order1.len() < eval_order2.len() {
        return eval_order1;
    }
    if eval_order2.len() < eval_order1.len() {
        return eval_order2;
    }
    // choose the one which evaluates most nodes the earliest
    let mut earilest_different_level = None;
    for i in 0..(cmp::min(eval_order1.len(), eval_order2.len())) {
        if eval_order1[i].len() != eval_order2[i].len() {
            earilest_different_level = Some(i);
            break;
        }
    }
    match earilest_different_level {
        Some(i) => {
            if eval_order1[i].len() > eval_order2[i].len() {
                eval_order1
            } else {
                eval_order2
            }
        }
        None => eval_order2,
    }
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
    for i in 0..=(2 * n) {
        cur.push(i);
        gen_combination(n, k, cur, result);
        cur.pop();
    }
}

fn merge_eval_orders(eval_orders: &Vec<Vec<Vec<Node>>>, starts: Vec<usize>) -> Vec<Vec<Node>> {
    assert!(eval_orders.len() == starts.len() + 1);
    let eval_orders: Vec<Vec<Vec<Node>>> = eval_orders
        .iter()
        .enumerate()
        .map(|(i, orders)| {
            if i > 0 {
                let mut extended: Vec<Vec<Node>> = vec![Vec::new(); starts[i - 1]];
                extended.extend(orders.clone());
                extended
            } else {
                orders.clone()
            }
        })
        .collect();
    let max_len = eval_orders
        .iter()
        .map(|eval_order| eval_order.len())
        .max()
        .unwrap();
    let extended_eval_orders: Vec<Vec<Vec<Node>>> = eval_orders
        .into_iter()
        .map(|eval_order| {
            let to_pad = max_len - eval_order.len();
            let padding: Vec<Vec<Node>> = vec![Vec::new(); to_pad];
            let mut eval_order = eval_order;
            eval_order.extend(padding);
            eval_order
        })
        .collect();
    let mut merged: Vec<Vec<Node>> = Vec::new();
    for i in 0..max_len {
        let level: Vec<Node> = extended_eval_orders
            .iter()
            .map(|eval_order| eval_order[i].clone())
            .fold(Vec::new(), |acc, item| {
                let mut acc = acc;
                acc.extend(item);
                acc
            });
        merged.push(level);
    }
    merged
        .into_iter()
        .filter(|level| !level.is_empty())
        .collect()
}

/// returns disjoint eval orders ordered_by their length in desc order
fn find_disjoint_eval_orders(mir: &RtLolaMir) -> Vec<Vec<Vec<Node>>> {
    let mut orders: Vec<Vec<Vec<Node>>> = get_roots(mir)
        .into_iter()
        .map(|roots| dag_eval_order(roots, mir))
        .collect();
    orders.sort_by(|a, b| b.len().cmp(&a.len()));
    orders
}

/// get roots of the graph after ignoring offset edges
/// roots are grouped together if they belog to the same connected sub-graph
fn get_roots(mir: &RtLolaMir) -> Vec<Vec<Node>> {
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
fn dag_eval_order(roots: Vec<Node>, mir: &RtLolaMir) -> Vec<Vec<Node>> {
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

pub fn prettify_eval_order(eval_order: &Vec<Vec<Node>>, mir: &RtLolaMir) -> Vec<String> {
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
}
