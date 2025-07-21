use crate::analysis::node::Node;

use crate::analysis::pipeline;
use crate::analysis::utils;
use crate::analysis::HardwareIR;
use rtlola_frontend::mir::MemorizationBound;
use rtlola_frontend::mir::RtLolaMir;
use rtlola_frontend::mir::Type;
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
    all_nodes.clone().into_iter().for_each(|node| {
        let window = node
            .get_children(mir)
            .into_iter()
            .filter(|(child, _)| all_nodes.contains(&child))
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

pub fn calculate_total_required_memory(eval_order: &Vec<Vec<Node>>, mir: &RtLolaMir) -> usize {
    let mut total: usize = 0;
    let memory = calculate_required_memory(eval_order, mir);
    for (nd, mem) in memory {
        total += get_datatype_memory_of_node(&nd, mir) * mem;
    }
    total
}

fn get_datatype_memory_of_node(node: &Node, mir: &RtLolaMir) -> usize {
    match node {
        Node::InputStream(x) => get_memory_by_datatype(&mir.inputs[x.clone()].ty),
        Node::OutputStream(x) => get_memory_by_datatype(&mir.outputs[x.clone()].ty),
        Node::SlidingWindow(x) => {
            let bits_in_bucket = get_memory_by_datatype(&mir.sliding_windows[x.clone()].ty);
            let total_buckets = get_sliding_window_size(x.clone(), &mir);
            total_buckets * bits_in_bucket
        }
    }
}

fn get_memory_by_datatype(typ: &Type) -> usize {
    match typ {
        Type::Int(_) => 64,
        Type::UInt(_) => 64,
        Type::Bool => 1,
        _ => unimplemented!(),
    }
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

/// ```
/// Instant to slide              time --->   
///   |  
///   |  
///   V  
/// slide1 -------- slide2 -------- slide3 -------- slide4 -------- slide5 --------
///   |      bkt1     |       bkt2    |      bkt3     |      bkt4     |  
///   x1      x2      x3       x4     |       x5      x6      x7      | x8
///   ------------------------------------------------------------------------
/// ```
///
///   If we need to work with the data within the window of 3 buckets   
///   Then at the instant of slide4 we need to aggregate the data `[x2, x3, x4, x5, x6]`    
///   `x1` was the data that came exactly at the instant when we were about to `slide1`  
///   this falls outside the window of 3 buckets at instant of `slide4`  
///   However `x2` & `x6` will be included in the aggregate  
///
///   So the semantic for a bucket is to be right inclusive and left exclusive  
///   i.e in bkt1 aggregation x2 & x3 are included whereas x1 is excluded  
///
///   Therefore we need 4 memory buckets to store all the required data  
///   even though we are calcuating aggregate on 3 buckets  
///
///   At the instant of `slide4`, before processing:  
///  ```
///   memory 1: (.., slide1] -> (x1)  
///   memory 2: (slide1, slide2] -> (x2, x3)  
///   memory 3: (slide2, slide3] -> (x4)  
///   memory 4: (slide3, slide4] -> (x5)  
///   ```
///
///   We put `x6` into the last bucket and then slide.   
///
///   So, after processing:  
///   ```
///   memory 1: (slide1, slide2] -> (x2, x3)  
///   memory 2: (slide2, slide3] -> (x4)  
///   memory 3: (slide3, slide4] -> (x5, x6)  
///   memory 4: (slide3, slide4] -> ()  
///  ```
///
pub fn get_sliding_window_size(window_idx: usize, mir: &RtLolaMir) -> usize {
    match mir.sliding_windows[window_idx].num_buckets {
        MemorizationBound::Bounded(x) => (x + 1) as usize,
        _ => unimplemented!(),
    }
}
