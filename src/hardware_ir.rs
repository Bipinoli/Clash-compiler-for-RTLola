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

use rtlola_frontend::mir::{
    Offset, Origin, PacingType, RtLolaMir, StreamAccessKind, StreamReference,
};

static DISPLAY_ALL_COMBINATIONS: bool = true;

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord, Serialize)]
enum Node {
    InputStream(usize),
    OutputStream(usize),
    SlidingWindow(usize),
    /// Phantom node acts as a placeholder to pad the eval order so that the disjoint order can only start from some offset  
    /// Node within the `Box<Node>` would be its immediate child  
    Phantom(Box<Node>),
}
impl Node {
    pub fn from_stream(stream_ref: &StreamReference) -> Node {
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
                    for (_, access_kind) in &child.1 {
                        let offset = match access_kind {
                            StreamAccessKind::Offset(off) => match off {
                                Offset::Past(x) => x.clone() as usize,
                                _ => unreachable!(),
                            },
                            _ => 0,
                        };
                        children.push((Node::from_access(child), offset));
                    }
                }
            }
            Node::OutputStream(x) => {
                for child in &mir.outputs[x.clone()].accessed_by {
                    for (_, access_kind) in &child.1 {
                        let offset = match access_kind {
                            StreamAccessKind::Offset(off) => match off {
                                Offset::Past(x) => x.clone() as usize,
                                _ => unreachable!(),
                            },
                            _ => 0,
                        };
                        children.push((Node::from_access(child), offset));
                    }
                }
            }
            Node::SlidingWindow(x) => {
                children.push((Node::from_stream(&mir.sliding_windows[x.clone()].caller), 0));
            }
            Node::Phantom(child) => {
                children.push((*child.clone(), 0));
            }
        }
        children
            .into_iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
    }

    fn get_non_offset_children(&self, mir: &RtLolaMir) -> Vec<Node> {
        let mut children: Vec<Node> = Vec::new();
        match self {
            Node::InputStream(x) => {
                for child in &mir.inputs[x.clone()].accessed_by {
                    for (_, access_kind) in &child.1 {
                        match access_kind {
                            StreamAccessKind::Offset(_) => {}
                            _ => {
                                children.push(Node::from_access(child));
                            }
                        }
                    }
                }
            }
            Node::OutputStream(x) => {
                for child in &mir.outputs[x.clone()].accessed_by {
                    for (_, access_kind) in &child.1 {
                        match access_kind {
                            StreamAccessKind::Offset(_) => {}
                            _ => {
                                children.push(Node::from_access(child));
                            }
                        }
                    }
                }
            }
            Node::SlidingWindow(x) => {
                children.push(Node::from_stream(&mir.sliding_windows[x.clone()].caller));
            }
            Node::Phantom(child) => {
                children.push(*child.clone());
            }
        }
        children
            .into_iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
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

    fn get_hold_parents(&self, mir: &RtLolaMir) -> Vec<Node> {
        let mut parents: Vec<Node> = Vec::new();
        match self {
            Node::OutputStream(x) => {
                for parent in &mir.outputs[x.clone()].accesses {
                    match parent.1.first().unwrap().1 {
                        StreamAccessKind::Hold => {
                            parents.push(Node::from_access(parent));
                        }
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
            Node::Phantom(_) => String::new(),
        }
    }
}

pub fn display_analysis(ir: &HardwareIR) {
    println!("\n------- The best pipeline --------");
    println!("{}\n", ir.prettify_eval_order().join("\n"));
    println!("{}\n", prettify_required_memory(ir).join("\n"));
    println!("{}\n", ir.visualize_pipeline(10).join("\n"));
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
                window_size(
                    &node,
                    &child,
                    pipeline_wait.clone(),
                    offset,
                    eval_order,
                    mir,
                )
            })
            .max()
            .unwrap_or(1);
        let until_output = window_size_to_remember_until_output(&node, pipeline_wait, eval_order);
        let needed = cmp::max(window, until_output);
        needed_memory.insert(node, needed);
    });
    needed_memory
}

pub fn prettify_required_memory(ir: &HardwareIR) -> Vec<String> {
    let mut retval: Vec<String> = Vec::new();
    for (nd, mem) in &ir.required_memory {
        retval.push(format!(
            "window {} = {}",
            nd.to_node().prettify(&ir.mir),
            mem
        ));
    }
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
    if offset > 0 {
        let parent_level = find_level(node, eval_order);
        let child_level = find_level(child, eval_order);
        if child_level > parent_level {
            child_level - parent_level + offset
        } else {
            offset
        }
    } else {
        let propagation_time = level_distance(node, child, eval_order).abs() as usize;
        let number_of_evals = ((propagation_time as f32) / (pipeline_wait as f32 + 1.0)).ceil();
        number_of_evals as usize
    }
}

fn window_size_to_remember_until_output(
    node: &Node,
    pipeline_wait: usize,
    eval_order: &Vec<Vec<Node>>,
) -> usize {
    match node {
        Node::OutputStream(_) => {
            let time_until_output = eval_order.len() - find_level(node, eval_order);
            let num_of_pipeline_evals_until_output =
                ((time_until_output as f32) / (pipeline_wait as f32 + 1.0)).ceil();
            num_of_pipeline_evals_until_output as usize
        }
        _ => 0,
    }
}

fn calculate_pipeline_wait(
    node: &Node,
    parent: &Node,
    offset: usize,
    eval_order: &Vec<Vec<Node>>,
    _mir: &RtLolaMir,
) -> usize {
    let propagation_time = level_distance(node, parent, eval_order);
    if propagation_time >= 0 {
        // parent is evaluated before child so no need to wait
        match propagation_time {
            0 if offset == 0 => {
                // Nodes evaluated at the same time can only have dependency for past value between them
                // i.e they can't have offset = 0
                usize::MAX
            }
            _ => 0,
        }
    } else {
        // child is evaluated before parent so we need to wait
        // Result is availabe in child after 1 cycle of input data reaching there
        let cur_val_avail_at_child_at = -propagation_time + 1;
        match offset {
            0 => {
                // Offset = 0 implies infinite feedback. Not a wellformed spec!
                usize::MAX
            }
            1 => (cur_val_avail_at_child_at - 1) as usize,
            _ => {
                // We can already run other pipelines in the meantime as the data depends on the past value
                // However the pipeline must be evenly distributed in timescale
                let total_pipelines_meanwhile = offset - 1;
                let evenly_distributed_pipeline_exec_time =
                    (cur_val_avail_at_child_at as f32) / (total_pipelines_meanwhile + 1) as f32;
                let next_immediate_pipeline_exec_at =
                    evenly_distributed_pipeline_exec_time.ceil() as usize;
                next_immediate_pipeline_exec_at - 1
            }
        }
    }
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
            let shift_from_offsets = node
                .get_offset_parents(mir)
                .into_iter()
                .map(|(parent, offset)| {
                    calculate_pipeline_wait(&node, &parent, offset, eval_order, mir)
                })
                .max()
                .unwrap_or(0);
            let shift_from_holds = {
                // There could be a cycle with holds
                //
                // For example:
                // input x: Int
                // output a := x + b.hold(or: 1)
                // output b @1kHz := a.hold(or: 0) + 1
                //
                // Here eval order is: x -> a -> b
                //
                // In such cases we take the last value of parent instead of the cur that will be evaluated later
                // to break the infinite cycle
                // i.e we take the parent value at offset 1
                node.get_hold_parents(mir)
                    .into_iter()
                    .map(|parent| {
                        let parent_eval_time = find_level(&parent, eval_order);
                        let child_eval_time = find_level(&node, eval_order);
                        let offset: usize = if parent_eval_time > child_eval_time {
                            1
                        } else {
                            0
                        };
                        calculate_pipeline_wait(&node, &parent, offset, eval_order, mir)
                    })
                    .max()
                    .unwrap_or(0)
            };
            cmp::max(shift_from_holds, shift_from_offsets)
        })
        .max();
    max_shift.unwrap_or(0)
}

/// Find the evaluation order of Nodes
///
/// - First find the eval orders ignoring the offsets  
/// - This can give us disjointed sub-graphs  
/// - If we don't want to pipeline then we can run them in parallel
/// - As in non-pipeline evaluation, the next eval cycle will only run after completely finishing earlier
/// - This gives us correct evaluation as all past values (offsets) will be available
/// - However, it might still be possible to pipeline the evaluation
/// - For this we could check the dependency by offsets and check the maximum pipeline_wait necessary
/// - The amount of necesary pipeline_wait also depends on when we want to start evaluation of the disjointed graphs
/// - For this we can try all the possible combinations and choose the one which requires the minimum pipeline_wait
///
/// For example:
/// ```
/// input x: Int
/// output a := x + 1
/// output b := a + 1
/// output c := b + 1
/// output d := c.offset(by: -1).defaults(to: 0)
/// ```
///
/// - After removing offsets the isolated graphs with eval orders are `[x, a, b, c]` and `[d]`
/// - We could try all combinations of eval orders i.e:
///     - `[(x, d), a, b, c]`
///     - `[x, (a, d), b, c]`
///     - `[x, a, (b, d), c]`
///     - `[x, a, b, (c, d)]`
///     - `[x, a, b, c, d]`
/// - Among them the eval order `[x, a, b, (c, d)]` & `[x, a, b, c, d]` will give the least `pipeline_wait` necessary i.e `0`
/// - However `[x, a, b, (c, d)]` is more desirable as the total eval layers are smaller and we get to evaluate d as early as possible
/// - When we look deeper, it makes sense as when evaluating `c` we will already have the past value of `c` availabe
/// - Whereas if we had tried to evaluate `d` along with `b` then we would need to wait one cycle until the `c` gets evaluated
/// - It is possible to avoid checking all possbile combinations using this observations
/// - However for the sake of simplicity we can simpley check all the combinations
/// - Given the time complexity of `O(n ^ (k + 1))` is acceptable
/// - where `n` = max length of eval order ~ number of nodes
///     - `k` = number of disjointed sub-graphs
///     - here we have `n` choice to start `k` eval-orders and each pipeline_wait calculation takes `O(n)` hence `k + 1` in the exponent
fn find_eval_order(mir: &RtLolaMir, display_all_combinations: bool) -> Vec<Vec<Node>> {
    let eval_orders = find_disjoint_eval_orders(mir);
    let n = eval_orders.first().unwrap().len();
    let all_starts = gen_all_combinations(n, eval_orders.len() - 1);
    let all_combinations: Vec<Vec<Vec<Node>>> = all_starts
        .into_iter()
        .map(|start| merge_eval_orders(&eval_orders, start))
        .collect::<HashSet<_>>()
        .into_iter()
        .map(|eval_order| {
            // filter out all phantom nodes
            eval_order
                .into_iter()
                .map(|level| {
                    level
                        .into_iter()
                        .filter(|nd| match nd {
                            Node::Phantom(_) => false,
                            _ => true,
                        })
                        .collect::<Vec<_>>()
                })
                .filter(|v| v.len() > 0)
                .collect::<Vec<Vec<Node>>>()
        })
        .collect();

    if display_all_combinations {
        all_combinations.iter().for_each(|eval_order| {
            println!();
            println!("{}\n", prettify_eval_order(&eval_order, mir).join("\n"));
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
        .map(|roots| {
            if has_cycle(roots.clone(), mir) {
                let (periodic_roots, non_periodic_roots) = break_cycle(roots, mir);
                let non_periodic_eval_order =
                    dag_eval_order(non_periodic_roots, mir, &|nd| !is_periodic(nd, mir));
                let periodic_eval_order = dag_eval_order(periodic_roots, mir, &|nd| {
                    is_periodic(nd, mir) || is_phantom(nd)
                });
                vec![non_periodic_eval_order, periodic_eval_order]
            } else {
                vec![dag_eval_order(roots, mir, &|_| true)]
            }
        })
        .flatten()
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
        let reachables = reachable_nodes(inputs, mir, &|_| true);
        mir.outputs
            .iter()
            .map(|output| Node::from_stream(&output.reference))
            .filter(|nd| !reachables.contains(nd))
            .collect()
    };
    let other_roots = remove_reachable_roots(unreachable_from_inputs, mir, &|_| true);
    roots.extend(other_roots);

    let reachables: Vec<HashSet<Node>> = roots
        .iter()
        .map(|nd| reachable_nodes(vec![nd.clone()], mir, &|_| true))
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

// evaluation order from a DAG (directed acyclic graph)
fn dag_eval_order(
    roots: Vec<Node>,
    mir: &RtLolaMir,
    condition: &dyn Fn(&Node) -> bool,
) -> Vec<Vec<Node>> {
    let mut order: Vec<Vec<Node>> = Vec::new();

    let mut cur_level: Vec<Node> = roots;
    let mut next_level: Vec<Node> = Vec::new();

    while !cur_level.is_empty() {
        order.push(cur_level.clone());
        for node in &cur_level {
            for child in node.get_non_offset_children(mir) {
                if condition(&child) {
                    next_level.push(child);
                }
            }
        }
        cur_level = remove_reachable_roots(next_level, mir, &condition);
        next_level = vec![];
    }
    order
}

fn remove_reachable_roots(
    roots: Vec<Node>,
    mir: &RtLolaMir,
    condition: &dyn Fn(&Node) -> bool,
) -> Vec<Node> {
    let all_reachables: HashSet<Node> = roots
        .iter()
        .map(|nd| {
            reachable_nodes(vec![nd.clone()], mir, &condition)
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
        .collect::<HashSet<_>>()
        .difference(&all_reachables)
        .map(|nd| nd.clone())
        .collect()
}

fn has_cycle(roots: Vec<Node>, mir: &RtLolaMir) -> bool {
    let mut visited: HashSet<Node> = HashSet::new();
    let mut visiting: HashSet<Node> = HashSet::new();
    for root in roots {
        if has_cycle_dfs(root, mir, &mut visiting, &mut visited) {
            return true;
        }
    }
    false
}

fn has_cycle_dfs(root: Node, mir: &RtLolaMir, visiting: &mut HashSet<Node>, visited: &mut HashSet<Node>) -> bool {
    if visited.contains(&root) {
        return false;
    }
    if visiting.contains(&root) {
        return true;
    }
    visiting.insert(root.clone());
    let children = root.get_non_offset_children(mir);
    for child in children {
        if has_cycle_dfs(child, mir, visiting, visited) {
            return true;
        }
    }
    visiting.remove(&root);
    visited.insert(root.clone());
    false
}


/// Isolate periodic and event-based streams in the subgraph to break the cycle  
/// This will create 2 or more isolated graphs from the subgraph  
/// Roots of such isolated graphs will be returned grouped by (periodic, non_periodic)
///
/// To break the cycle we use the rule of evaluating event-based nodes before periodic (existing rule in RTLola).   
/// So, the periodic nodes splitted here must only be evaluated after all the event-based nodes are evaluated   
/// For that we introduce chains of phantom nodes as parents for periodic nodes to offset the eval order properly during merge
/// Note that the offset is only necessary in this subgraph we are breaking  
/// There could be a valid periodic root in a different subgraph without a cycle that doesn't need to be offsetted  
fn break_cycle(roots: Vec<Node>, mir: &RtLolaMir) -> (Vec<Node>, Vec<Node>) {
    let all_nodes_in_subgraph = reachable_nodes(roots, mir, &|_| true);
    let periodic_nodes: HashSet<Node> = all_nodes_in_subgraph
        .iter()
        .filter(|&nd| is_periodic(nd, mir))
        .map(|nd| nd.clone())
        .collect();
    let non_periodic_nodes: Vec<Node> = all_nodes_in_subgraph
        .difference(&periodic_nodes)
        .map(|nd| nd.clone())
        .collect();

    let periodic_roots =
        remove_reachable_roots(periodic_nodes.into_iter().collect::<Vec<_>>(), mir, &|nd| {
            is_periodic(nd, mir)
        });
    let non_periodic_roots =
        remove_reachable_roots(non_periodic_nodes, mir, &|nd| !is_periodic(nd, mir));

    let eval_order_len_from_non_periodic =
        dag_eval_order(non_periodic_roots.clone(), mir, &|nd| !is_periodic(nd, mir)).len();
    let offsetted_periodic_roots: Vec<Node> = periodic_roots
        .into_iter()
        .map(|nd| {
            let mut offset_cnt = eval_order_len_from_non_periodic.clone();
            let mut node = nd.clone();
            while offset_cnt > 0 {
                node = Node::Phantom(Box::new(node));
                offset_cnt -= 1;
            }
            node
        })
        .collect();

    (offsetted_periodic_roots, non_periodic_roots)
}

fn reachable_nodes(
    roots: Vec<Node>,
    mir: &RtLolaMir,
    condition: &dyn Fn(&Node) -> bool,
) -> HashSet<Node> {
    let mut rechables: HashSet<Node> = HashSet::new();
    let mut stack: Vec<Node> = roots;
    while !stack.is_empty() {
        let node = stack.pop().unwrap();
        rechables.insert(node.clone());
        for child in node.get_non_offset_children(mir) {
            if condition(&child) && !rechables.contains(&child) {
                stack.push(child);
            }
        }
    }
    rechables
}

fn is_periodic(node: &Node, mir: &RtLolaMir) -> bool {
    match node {
        Node::OutputStream(x) => match mir.outputs[x.clone()].eval.eval_pacing {
            PacingType::GlobalPeriodic(_) => true,
            PacingType::Event(_) => false,
            PacingType::Constant => false,
            _ => unimplemented!(),
        },
        _ => false,
    }
}

fn is_phantom(node: &Node) -> bool {
    match node {
        Node::Phantom(_) => true,
        _ => false,
    }
}

fn find_level(node: &Node, eval_order: &Vec<Vec<Node>>) -> usize {
    for (i, nodes) in eval_order.iter().enumerate() {
        if nodes.contains(node) {
            return i;
        }
    }
    unreachable!()
}

fn prettify_eval_order(eval_order: &Vec<Vec<Node>>, mir: &RtLolaMir) -> Vec<String> {
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

fn visualize_pipeline(
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

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord, Serialize)]
pub enum EvalNode {
    InputStream(usize),
    OutputStream(usize),
    SlidingWindow(usize),
}
impl EvalNode {
    fn from_node(node: &Node) -> Self {
        match node {
            Node::InputStream(x) => EvalNode::InputStream(x.clone()),
            Node::OutputStream(x) => EvalNode::OutputStream(x.clone()),
            Node::SlidingWindow(x) => EvalNode::SlidingWindow(x.clone()),
            Node::Phantom(_) => unreachable!(),
        }
    }
    fn to_node(&self) -> Node {
        match self {
            EvalNode::InputStream(x) => Node::InputStream(x.clone()),
            EvalNode::OutputStream(x) => Node::OutputStream(x.clone()),
            EvalNode::SlidingWindow(x) => Node::SlidingWindow(x.clone()),
        }
    }
    pub fn from_stream(stream_ref: &StreamReference) -> EvalNode {
        EvalNode::from_node(&Node::from_stream(stream_ref))
    }
    pub fn prettify(&self, mir: &RtLolaMir) -> String {
        self.to_node().prettify(mir)
    }
}

#[derive(PartialEq, Clone, Debug, Serialize)]
pub struct HardwareIR {
    pub mir: RtLolaMir,
    pub evaluation_order: Vec<Vec<EvalNode>>,
    pub pipeline_wait: usize,
    pub required_memory: HashMap<EvalNode, usize>,
    pub spec: String,
    pub spec_name: String,
    pub debug: bool,
}

impl HardwareIR {
    pub fn new(mir: RtLolaMir, spec: String, spec_name: String, debug: bool) -> Self {
        let eval_order = find_eval_order(&mir, false);
        let pipeline_wait = calculate_necessary_pipeline_wait(&eval_order, &mir);
        let required_memory = calculate_required_memory(&eval_order, &mir);
        HardwareIR {
            evaluation_order: eval_order
                .into_iter()
                .map(|level| {
                    level
                        .into_iter()
                        .map(|nd| EvalNode::from_node(&nd))
                        .collect::<Vec<EvalNode>>()
                })
                .collect(),
            mir,
            pipeline_wait,
            required_memory: required_memory.into_iter().fold(
                HashMap::new(),
                |mut acc, (nd, mem)| {
                    acc.insert(EvalNode::from_node(&nd), mem);
                    acc
                },
            ),
            spec,
            spec_name,
            debug,
        }
    }

    pub fn find_level(&self, node: &EvalNode) -> usize {
        for (i, nodes) in self.evaluation_order.iter().enumerate() {
            if nodes.contains(node) {
                return i;
            }
        }
        unreachable!()
    }

    pub fn prettify_eval_order(&self) -> Vec<String> {
        let eval_order: Vec<Vec<Node>> = self
            .evaluation_order
            .iter()
            .map(|level| level.iter().map(|nd| nd.to_node()).collect::<Vec<_>>())
            .collect();
        prettify_eval_order(&eval_order, &self.mir)
    }

    pub fn visualize_pipeline(&self, time_steps: usize) -> Vec<String> {
        let eval_order: Vec<Vec<Node>> = self
            .evaluation_order
            .iter()
            .map(|level| level.iter().map(|nd| nd.to_node()).collect::<Vec<_>>())
            .collect();
        visualize_pipeline(&eval_order, self.pipeline_wait, time_steps, &self.mir)
    }
}
