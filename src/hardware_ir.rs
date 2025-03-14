use std::collections::{HashMap, HashSet};

use rtlola_frontend::{self as RF, mir::StreamReference};

use crate::utils;

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord)]
enum EvalNode {
    InputStream(usize),
    InputWindow(usize),
    OutputStream(usize),
    SlidingWindow(usize),
}

#[derive(PartialEq, Clone)]
struct HardwareIR {
    mir: RF::RtLolaMir,
    evaluation_order: Vec<Vec<EvalNode>>,
}

impl HardwareIR {
    fn new(mir: RF::RtLolaMir) -> Self {
        HardwareIR {
            mir: mir,
            evaluation_order: vec![],
        }
    }
}

fn merge_eval_orders(orders: Vec<Vec<EvalNode>>) -> Vec<Vec<EvalNode>> {
    let mut set = HashSet::new();
    for order in orders.clone() {
        for node in order {
            set.insert(node);
        }
    }
    let mut keys: Vec<EvalNode> = set.into_iter().collect();
    keys.sort_by(|a, b| a.cmp(&b));
    let mut map = HashMap::new();
    let mut reverse_map = HashMap::new();
    for (i, item) in keys.into_iter().enumerate() {
        map.insert(item.clone(), i);
        reverse_map.insert(i, item);
    }
    let orders: Vec<Vec<usize>> = orders
        .into_iter()
        .map(|v| {
            v.into_iter()
                .map(|a| map.get(&a).unwrap().clone())
                .collect()
        })
        .collect();
    let merged_orders = utils::merge_orders(orders);
    let merged_orders: Vec<Vec<EvalNode>> = merged_orders
        .into_iter()
        .map(|v| {
            v.into_iter()
                .map(|i| reverse_map.get(&i).unwrap().clone())
                .collect()
        })
        .collect();
    merged_orders
}

fn eval_orders_from_sliding_windows(mir: &RF::RtLolaMir) -> Vec<Vec<EvalNode>> {
    let mut orders: Vec<Vec<EvalNode>>  = vec![];
    for sw in &mir.sliding_windows {
        // target -> window -> caller
        let target = match sw.target {
            StreamReference::In(x) => EvalNode::InputStream(x),
            StreamReference::Out(x) => EvalNode::OutputStream(x),
        };
        let window = EvalNode::SlidingWindow(sw.reference.idx());
        let caller = match sw.caller {
            StreamReference::In(x) => EvalNode::InputStream(x),
            StreamReference::Out(x) => EvalNode::OutputStream(x),
        };
        orders.push(vec![target, window, caller]);
    }
    orders
}

fn eval_orders_from_outputs(mir: &RF::RtLolaMir) -> Vec<Vec<EvalNode>> {
    let mut orders: Vec<Vec<EvalNode>>  = vec![];
    for out in &mir.outputs {
        // whatever `out` accesses must be evaluated before
        
    }
    orders
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge_eval_orders_works() {
        // from sliding window analysis
        let mut orders = vec![
            vec![
                EvalNode::OutputStream(1),
                EvalNode::SlidingWindow(0),
                EvalNode::OutputStream(2),
            ],
            vec![
                EvalNode::OutputStream(2),
                EvalNode::SlidingWindow(2),
                EvalNode::OutputStream(4),
            ],
            vec![
                EvalNode::OutputStream(0),
                EvalNode::SlidingWindow(1),
                EvalNode::OutputStream(3),
            ],
        ];
        // from outputs analysis
        let output_orders = vec![
            vec![EvalNode::InputWindow(0), EvalNode::OutputStream(0)],
            vec![EvalNode::OutputStream(0), EvalNode::OutputStream(1)],
            vec![EvalNode::SlidingWindow(0), EvalNode::OutputStream(2)],
            vec![EvalNode::SlidingWindow(1), EvalNode::OutputStream(3)],
            vec![EvalNode::SlidingWindow(2), EvalNode::OutputStream(4)],
        ];
        orders.extend(output_orders);

        let expected = vec![
            vec![EvalNode::InputWindow(0)],
            vec![EvalNode::OutputStream(0)],
            vec![EvalNode::OutputStream(1), EvalNode::SlidingWindow(1)],
            vec![EvalNode::OutputStream(3), EvalNode::SlidingWindow(0)],
            vec![EvalNode::OutputStream(2)],
            vec![EvalNode::SlidingWindow(2)],
            vec![EvalNode::OutputStream(4)],
        ];

        let actual = merge_eval_orders(orders);
        assert_eq!(actual, expected);
    }
}
