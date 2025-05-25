use crate::analysis::node::Node;

use rtlola_frontend::mir::RtLolaMir;
use std::usize;

use crate::analysis::eval_order::utils;

fn calculate_pipeline_wait(
    node: &Node,
    parent: &Node,
    offset: usize,
    eval_order: &Vec<Vec<Node>>,
    _mir: &RtLolaMir,
) -> usize {
    let propagation_time = utils::level_distance(node, parent, eval_order);
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

pub fn calculate_necessary_pipeline_wait(eval_order: &Vec<Vec<Node>>, mir: &RtLolaMir) -> usize {
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
