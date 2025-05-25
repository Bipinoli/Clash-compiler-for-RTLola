use crate::analysis::node::Node;

use crate::analysis::pipeline;
use crate::analysis::utils;
use crate::analysis::HardwareIR;
use rtlola_frontend::mir::RtLolaMir;
use std::{collections::HashMap, usize};

pub fn prettify_required_memory(ir: &HardwareIR) -> Vec<String> {
    let mut retval: Vec<String> = Vec::new();
    for (nd, mem) in &ir.required_memory {
        retval.push(format!("window {} = {}", nd.prettify(&ir.mir), mem));
    }
    retval
}

pub fn calculate_required_memory(
    eval_order: &Vec<Vec<Node>>,
    mir: &RtLolaMir,
) -> HashMap<Node, usize> {
    let mut needed_memory: HashMap<Node, usize> = HashMap::new();
    let pipeline_wait = pipeline::calculate_necessary_pipeline_wait(eval_order, mir);
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
        let needed = std::cmp::max(window, until_output);
        needed_memory.insert(node, needed);
    });
    needed_memory
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
        let parent_level = utils::find_level(node, eval_order);
        let child_level = utils::find_level(child, eval_order);
        if child_level > parent_level {
            child_level - parent_level + offset
        } else {
            offset
        }
    } else {
        let propagation_time = utils::level_distance(node, child, eval_order).abs() as usize;
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
            let time_until_output = eval_order.len() - utils::find_level(node, eval_order);
            let num_of_pipeline_evals_until_output =
                ((time_until_output as f32) / (pipeline_wait as f32 + 1.0)).ceil();
            num_of_pipeline_evals_until_output as usize
        }
        _ => 0,
    }
}
