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

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord, Serialize)]
pub enum Node {
    InputStream(usize),
    OutputStream(usize),
    SlidingWindow(usize),
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
        }
    }
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
        retval.push(format!("window {} = {}", nd.prettify(&ir.mir), mem));
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

fn find_eval_order(mir: &RtLolaMir) -> Vec<Vec<Node>> {
    // Observations:
    // 1. Without cycle the evaluation order is simply the level order of the DAG
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
    // 3. Get eval order from the resulting DAGs and merge them according to -ve offsets & event-based/periodic periority rule
    //
    // Algorithm:
    // step 1: separate event-based and periodic
    // step 2: Split further ignoring -ve offsets
    // step 3: In each group find roots and group them if they are connected ignoring -ve offset
    // step 4: calculate eval order and pipeline wait in all groups (periodic, non periodic)
    // step 5: max pipeline_wait among every eval orders will be the total pipeline_wait of the while eval order
    //         assert that the max pipeline wait <= max needed by any -ve cycle i.e max path length
    // step 6: merge eval-orders within event-based and within periodic accoring to -ve offset & pipeline_wait
    //         try to evaluate as early as possible without increasing the pipeline_wait
    // step 7: after all event based are merged and all periodic are merged together then merge periodic & event_based together
    //         merged periodic & event-based together making sure the event-based node depending on periodic gets evaluated earlier
    //         i.e the existing event-based before periodic semantics in RTLola
    // Note:
    // How we merge eval orders affects "pipeline_wait", total levels in eval_order & the required memory of each node
    // We should minimize them in the order: "pipeline_wait", "memory", "total levels"
    // i.e we prefer as small pipeline_wait and possible. If the pipeline wait is the same we prefer least possible memory and so on.

    let (event_based_nodes, periodic_nodes) = split_event_based_and_periodic(mir);
    let is_event_based_node = |nd: &Node| !is_periodic(&nd, mir);
    let is_periodc_node = |nd: &Node| is_periodic(&nd, mir);

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

    let pipeline_wait = vec![&orders_ev_based[..], &orders_periodic[..]]
        .concat()
        .iter()
        .map(|eval_order| calculate_necessary_pipeline_wait(eval_order, mir))
        .max()
        .unwrap_or(0);

    //TODO: 
    // merge_by_offset
    // merge_periodic_and_event_based

    unimplemented!()
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
        .filter(|&nd| is_periodic(nd, mir))
        .map(|nd| nd.clone())
        .collect();
    let event_based: Vec<Node> = all_nodes
        .iter()
        .filter(|&nd| !is_periodic(nd, mir))
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
        let rechables = reachable_nodes(vec![node.clone()], mir, node_cond);
        roots = roots.difference(&rechables).map(|nd| nd.clone()).collect();
    }
    let mut connected_components: HashMap<Node, Vec<Node>> = HashMap::new();
    for root in roots {
        let comp_leader = last_leaf(root.clone(), mir, node_cond);
        let updated = connected_components
            .get(&comp_leader)
            .unwrap_or(&Vec::new())
            .clone();
        connected_components.insert(comp_leader, updated);
    }
    connected_components.values().map(|v| v.clone()).collect()
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

// evaluation order from a DAG (directed acyclic graph)
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

fn is_periodic(node: &Node, mir: &RtLolaMir) -> bool {
    match node {
        Node::InputStream(_) => false,
        Node::OutputStream(x) => match mir.outputs[x.clone()].eval.eval_pacing {
            PacingType::GlobalPeriodic(_) => true,
            PacingType::Event(_) => false,
            PacingType::Constant => false,
            _ => unimplemented!(),
        },
        Node::SlidingWindow(_) => true,
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

#[derive(PartialEq, Clone, Debug, Serialize)]
pub struct HardwareIR {
    pub mir: RtLolaMir,
    pub evaluation_order: Vec<Vec<Node>>,
    pub pipeline_wait: usize,
    pub required_memory: HashMap<Node, usize>,
    pub spec: String,
    pub spec_name: String,
    pub debug: bool,
}

impl HardwareIR {
    pub fn new(
        mir: RtLolaMir,
        spec: String,
        spec_name: String,
        debug: bool,
        verbose: bool,
    ) -> Self {
        let eval_order = find_eval_order(&mir);
        let pipeline_wait = calculate_necessary_pipeline_wait(&eval_order, &mir);
        let required_memory = calculate_required_memory(&eval_order, &mir);
        HardwareIR {
            evaluation_order: eval_order,
            mir,
            pipeline_wait,
            required_memory,
            spec,
            spec_name,
            debug,
        }
    }

    pub fn find_level(&self, node: &Node) -> usize {
        find_level(node, &self.evaluation_order)
    }

    pub fn prettify_eval_order(&self) -> Vec<String> {
        prettify_eval_order(&self.evaluation_order, &self.mir)
    }

    pub fn visualize_pipeline(&self, time_steps: usize) -> Vec<String> {
        visualize_pipeline(
            &self.evaluation_order,
            self.pipeline_wait,
            time_steps,
            &self.mir,
        )
    }

    pub fn display_analysis(&self) {
        println!("\n------- The best pipeline --------");
        println!("{}\n", self.prettify_eval_order().join("\n"));
        println!("{}\n", prettify_required_memory(&self).join("\n"));
        println!("{}\n", self.visualize_pipeline(10).join("\n"));
    }
}
