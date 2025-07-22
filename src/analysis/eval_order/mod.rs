use crate::analysis::node::Node;
use rtlola_frontend::mir::RtLolaMir;
use std::{collections::HashSet, vec};

pub mod memory;
mod merge_combinations;
pub mod pipeline;
pub mod utils;

pub fn find_eval_order(mir: &RtLolaMir) -> Vec<Vec<Node>> {
    // Observations:
    // 1. Without cycle the evaluation order is simply the level order of the DAG (directed acyclic graph)
    // 2. Cycle can exist in 3 ways:
    //      a) Between event-based streams via -ve offset
    //      b) Between periodic streams via -ve offset
    //      c) Across event-based & periodic streams via hold or sliding window
    // 3. -ve offset can also exist in a DAG without a cycle
    // 4. With only -ve offset dependency, the child can be evaluated earlier with or even before the parent as the value is alreay there
    // 5. The longest path along a cycle in a subgraph determines how long we have to wait (pipelin_wait)  before the next pipeline can be started
    //
    // Idea:
    // 1. Break cycles across event-based & periodic by splitting graph into only event-based & periodic streams
    // 2. Break cycle due to -ve offset by ignoring -ve offsets
    // 3. Get eval order from the resulting DAGs and merge them according to -ve offsets & event-based/periodic priority rule
    //
    // Algorithm:
    // step 1: Separate event-based and periodic
    // step 2: Split further ignoring -ve offsets
    // step 3: In each group find roots and group them if they are connected ignoring -ve offset
    // step 4: Calculate eval order and pipeline wait in all groups (periodic, non periodic)
    // step 5: max pipeline_wait among every eval orders will be the total pipeline_wait of the whole eval order
    // step 6: Merge eval-orders within event-based and within periodic accoring to -ve offset & pipeline_wait
    //         try to evaluate as early as possible without increasing the pipeline_wait
    // step 7: After all event-based are merged and all periodic are merged, then we merge them together
    //         RTLola has the semantics of evaluation event-based before periodic to avoid infinite cycle
    //         So, merge them accordingly
    // Note:
    // How we merge eval orders affects "pipeline_wait", total levels in eval_order & the required memory of each node
    // We should try to minimize them in the order: "pipeline_wait", "memory", "total levels"
    // i.e we prefer as small pipeline_wait and possible. If the pipeline wait is the same we prefer least possible memory and so on.

    let (event_based_nodes, periodic_nodes) = split_event_based_and_periodic(mir);
    let is_event_based_node = |nd: &Node| !nd.is_periodic(mir);
    let is_periodc_node = |nd: &Node| nd.is_periodic(mir);

    let event_based_roots = find_roots(event_based_nodes, mir, &is_event_based_node);
    let periodic_roots = find_roots(periodic_nodes, mir, &is_periodc_node);

    let orders_ev_based: Vec<_> = event_based_roots
        .iter()
        .map(|roots| dag_eval_order(roots.clone(), mir, &is_event_based_node))
        .collect();
    let orders_periodic: Vec<_> = periodic_roots
        .iter()
        .map(|roots| dag_eval_order(roots.clone(), mir, &is_periodc_node))
        .collect();

    let merged_event_based =
        merge_eval_orders_by_offset(orders_ev_based, mir, &is_event_based_node);
    let merged_periodic = merge_eval_orders_by_offset(orders_periodic, mir, &is_periodc_node);

    merge_periodic_and_event_based_eval_orders(merged_event_based, merged_periodic, mir)
}

fn split_event_based_and_periodic(mir: &RtLolaMir) -> (Vec<Node>, Vec<Node>) {
    let all_nodes: Vec<Node> = {
        let inputs: Vec<Node> = mir
            .inputs
            .iter()
            .enumerate()
            .map(|(i, _)| Node::InputStream(i))
            .collect();
        let outputs: Vec<Node> = mir
            .outputs
            .iter()
            .enumerate()
            .map(|(i, _)| Node::OutputStream(i))
            .collect();
        let sliding_windows: Vec<Node> = mir
            .sliding_windows
            .iter()
            .enumerate()
            .map(|(i, _)| Node::SlidingWindow(i))
            .collect();
        vec![&inputs[..], &outputs[..], &sliding_windows[..]].concat()
    };
    let periodic: Vec<Node> = all_nodes
        .iter()
        .filter(|&nd| nd.is_periodic(mir))
        .map(|nd| nd.clone())
        .collect();
    let event_based: Vec<Node> = all_nodes
        .iter()
        .filter(|&nd| !nd.is_periodic(mir))
        .map(|nd| nd.clone())
        .collect();
    (event_based, periodic)
}

fn find_roots(
    nodes: Vec<Node>,
    mir: &RtLolaMir,
    node_cond: &dyn Fn(&Node) -> bool,
) -> Vec<Vec<Node>> {
    let mut roots: HashSet<Node> = nodes.iter().map(|nd| nd.clone()).collect();
    for node in nodes {
        if roots.contains(&node) {
            let rechables = reachable_nodes(vec![node.clone()], mir, node_cond);
            roots = roots.difference(&rechables).map(|nd| nd.clone()).collect();
            roots.insert(node.clone());
        }
    }
    let mut connected_roots: Vec<Vec<Node>> = Vec::new();
    let mut already_connected: Vec<bool> = vec![false; roots.len()];
    for (i, r1) in roots.iter().enumerate() {
        if !already_connected[i] {
            let mut connected: Vec<Node> = vec![r1.clone()];
            for (j, r2) in roots.iter().enumerate().skip(i + 1) {
                if !already_connected[j] {
                    if are_connected(r1.clone(), r2.clone(), mir, node_cond) {
                        connected.push(r2.clone());
                        already_connected[j] = true;
                    }
                }
            }
            connected_roots.push(connected);
        }
    }
    connected_roots
}

fn are_connected(
    node1: Node,
    node2: Node,
    mir: &RtLolaMir,
    node_cond: &dyn Fn(&Node) -> bool,
) -> bool {
    let last_leaf1 = last_leaf(node1.clone(), mir, node_cond);
    let last_leaf2 = last_leaf(node2.clone(), mir, node_cond);
    let earliest_parents1 = earliest_parents(last_leaf1, mir, node_cond);
    let earliest_parents2 = earliest_parents(last_leaf2, mir, node_cond);
    earliest_parents1.intersection(&earliest_parents2).count() > 0
}

fn last_leaf(root: Node, mir: &RtLolaMir, node_cond: &dyn Fn(&Node) -> bool) -> Node {
    let mut retval: Node = root.clone();
    for child in root.get_non_offset_children(mir) {
        if node_cond(&child) {
            retval = last_leaf(child, mir, node_cond);
        }
    }
    retval
}

fn earliest_parents(
    root: Node,
    mir: &RtLolaMir,
    node_cond: &dyn Fn(&Node) -> bool,
) -> HashSet<Node> {
    let mut parents: HashSet<Node> = HashSet::new();
    root.get_non_offset_parents(mir)
        .iter()
        .filter(|&parent| node_cond(parent))
        .for_each(|parent| {
            parents.extend(earliest_parents(parent.clone(), mir, node_cond));
        });
    if parents.is_empty() {
        parents.insert(root);
    }
    parents
}

/// evaluation order from a DAG (directed acyclic graph)  
fn dag_eval_order(
    roots: Vec<Node>,
    mir: &RtLolaMir,
    node_cond: &dyn Fn(&Node) -> bool,
) -> Vec<Vec<Node>> {
    let mut order: Vec<Vec<Node>> = Vec::new();

    let mut cur_level: Vec<Node> = roots;
    let mut next_level: Vec<Node> = Vec::new();

    while !cur_level.is_empty() {
        order.push(cur_level.clone());
        for node in &cur_level {
            for child in node.get_non_offset_children(mir) {
                if node_cond(&child) {
                    next_level.push(child);
                }
            }
        }
        cur_level = remove_reachable_roots(next_level, mir, &node_cond);
        next_level = vec![];
    }
    order
}

fn remove_reachable_roots(
    roots: Vec<Node>,
    mir: &RtLolaMir,
    node_cond: &dyn Fn(&Node) -> bool,
) -> Vec<Node> {
    let all_reachables: HashSet<Node> = roots
        .iter()
        .map(|nd| {
            reachable_nodes(vec![nd.clone()], mir, &node_cond)
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

fn merge_eval_orders_by_offset(
    orders: Vec<Vec<Vec<Node>>>,
    mir: &RtLolaMir,
    _node_cond: &dyn Fn(&Node) -> bool,
) -> Vec<Vec<Node>> {
    // It is a heuristic approach that doesn't guarantee an optimum order
    //
    // For simplicity, we enforce a restriction of not allowing stretching of individual orders
    // For example:
    // If we have orders: [a, b, c, d, e] & [m, n, o] to merge and if we allow strectching
    // They could be merged in various ways:
    // Option 1:
    // [a, b, c, d, e]
    // [m, _, _, n, o] <-- [m,n,o] is stretched
    // It means, a & m are in the same level, and so on
    //
    // Option 2:
    // [a, b, c, d, e]
    // [_, m, n, o, _] <-- [m,n,o] is not stretched i.e the order is tightly packed
    // etc.
    //
    // Allowing for stretching let's use further optimize the pipeline_wait, memory and total evaluation levels
    //
    // For example, think of a situation when some parent before these orders depend on `m` by -ve offset
    // and say `n` depends on some child after these orders by -ve offset
    // In that case, option 1 could give us smaller pipeline_wait as the path between those -ve offset nodes would be smaller
    //
    // Producing an optimum order here requires thinking of various situations which is outside the scope for now
    // So we restrict the stretching
    //
    // There is also a case of -ve edges that don't create a cycle
    // For example: a ---> b ----> c --(-2)--> d
    // Here, `d` needs the value of `c`, 2 eval cycles ago
    // Which means when the pipeline is about to evaluate `a` the value in `c` would be from lastest the 2 cycles ago
    // So, `d` could be evaluated as early as with `a` without introducing any extra pipeline_wait
    // However, there could also be further children from `d` that also depends on `c`
    // so when we pull `d` at the front the distance between `d` and those children would increase
    // Depending on -ve offset there, this could actually increase the pipeline_wait and the amount of things to remember in nodes
    //
    // So for simplicity we follow the heuristics of merging the orders by starting at different offsets
    // We try all possible combinations when the stretching is not allowed and choose the merge with the lease pipeline_wait

    merge_combinations::merge_eval_orders_various_combinations(orders, mir)
}

fn merge_periodic_and_event_based_eval_orders(
    event_based: Vec<Vec<Node>>,
    periodic: Vec<Vec<Node>>,
    mir: &RtLolaMir,
) -> Vec<Vec<Node>> {
    // Evaluating periodic nodes after all event-based is always valid (RTlola event-based node before periodic node rule)
    // However, we can do better as periodic nodes don't have to wait for those event-based nodes which are not connected via data-dependency
    // For simplicity we also don't allow stretching of eval orders here
    // And follow the heuristics of finding the earliest level where the periodic eval order could be started
    merge_periodic_and_event_based(
        event_based.clone(),
        periodic.clone(),
        get_lowest_offset(&event_based, &periodic, mir),
    )
}

fn get_lowest_offset(
    event_based: &Vec<Vec<Node>>,
    periodic: &Vec<Vec<Node>>,
    mir: &RtLolaMir,
) -> usize {
    // We could just iterate down from the highest offset till the invalid point is reached
    // However as this is monotonic, the threshold can be more efficiently found with binary search
    let edges_between = all_edges_between_event_based_and_periodic(periodic, event_based, mir);
    let hi = event_based.len() as i32;
    let is_valid =
        |offset: i32| is_valid_merge(offset as usize, periodic, event_based, &edges_between);
    binary_search_invalid_threshold(hi, &is_valid) as usize
}

fn binary_search_invalid_threshold(hi: i32, is_valid: &dyn Fn(i32) -> bool) -> i32 {
    let mut lo: i32 = -1;
    let mut hi: i32 = hi;
    while lo + 1 < hi {
        let md = (lo + hi) / 2;
        if is_valid(md) {
            hi = md;
        } else {
            lo = md;
        }
    }
    lo + 1
}

fn is_valid_merge(
    periodic_offset: usize,
    periodic: &Vec<Vec<Node>>,
    event_based: &Vec<Vec<Node>>,
    edges_between: &Vec<Edge>,
) -> bool {
    // If there is an edge between event-based and periodic, then the event-based node must be evaluated before periodic
    for edge in edges_between {
        let periodic_node_level = periodic_offset + utils::find_level(&edge.periodic, &periodic);
        let event_based_node_level = utils::find_level(&edge.event_based, &event_based);
        if periodic_node_level <= event_based_node_level {
            return false;
        }
    }
    true
}

#[derive(Debug)]
struct Edge {
    periodic: Node,
    event_based: Node,
}

fn all_edges_between_event_based_and_periodic(
    periodic_eval_order: &Vec<Vec<Node>>,
    event_based_eval_order: &Vec<Vec<Node>>,
    mir: &RtLolaMir,
) -> Vec<Edge> {
    let periodic_nodes: HashSet<Node> = periodic_eval_order.clone().into_iter().flatten().collect();
    let event_based_nodes: HashSet<Node> = event_based_eval_order
        .clone()
        .into_iter()
        .flatten()
        .collect();
    let mut edges: Vec<Edge> = Vec::new();
    for periodic_node in &periodic_nodes {
        for (child, _) in periodic_node.get_children(mir) {
            if event_based_nodes.contains(&child) {
                edges.push(Edge {
                    periodic: periodic_node.clone(),
                    event_based: child.clone(),
                });
            }
        }
    }
    for event_based_node in &event_based_nodes {
        for (child, _) in event_based_node.get_children(mir) {
            if periodic_nodes.contains(&child) {
                edges.push(Edge {
                    periodic: child.clone(),
                    event_based: event_based_node.clone(),
                });
            }
        }
    }
    edges
}

fn merge_periodic_and_event_based(
    event_based: Vec<Vec<Node>>,
    periodic: Vec<Vec<Node>>,
    periodic_offset: usize,
) -> Vec<Vec<Node>> {
    let new_length = std::cmp::max(periodic_offset + periodic.len(), event_based.len());
    let (padded_event_based, padded_periodic) = {
        if new_length > event_based.len() {
            let ev_padding = new_length - event_based.len();
            let pr_padding = periodic_offset;
            (
                vec![&event_based[..], &vec![vec![]; ev_padding][..]].concat(),
                vec![&vec![vec![]; pr_padding][..], &periodic[..]].concat(),
            )
        } else {
            let padding1 = periodic_offset;
            let padding2 = new_length - (periodic.len() + padding1);
            (
                event_based,
                vec![
                    &vec![vec![]; padding1][..],
                    &periodic[..],
                    &vec![vec![]; padding2][..],
                ]
                .concat(),
            )
        }
    };
    padded_event_based
        .iter()
        .zip(padded_periodic)
        .map(|(v1, v2)| {
            let mut v = v1.clone();
            v.extend(v2);
            v
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binary_search_invalid_threshold_works() {
        for hi in 0..10 {
            for expected in 0..hi + 1 {
                let is_valid = |v: i32| v >= expected;
                let ans = binary_search_invalid_threshold(hi, &is_valid);
                assert_eq!(
                    ans, expected,
                    "Expected: {expected}, Actual: {ans}, with hi: {hi}"
                );
            }
        }
    }
}
