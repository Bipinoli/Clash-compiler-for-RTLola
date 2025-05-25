use crate::analysis::node::Node;

use rtlola_frontend::mir::RtLolaMir;
use std::usize;

pub fn level_distance(a: &Node, b: &Node, eval_order: &Vec<Vec<Node>>) -> i32 {
    let level_a = find_level(a, eval_order) as i32;
    let level_b = find_level(b, eval_order) as i32;
    level_a - level_b
}

pub fn find_level(node: &Node, eval_order: &Vec<Vec<Node>>) -> usize {
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
