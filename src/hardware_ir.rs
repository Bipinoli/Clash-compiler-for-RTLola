use std::{cmp::Ordering, collections::{HashMap, HashSet}};

use rtlola_frontend as RF;

use crate::utils;

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord)]
enum EvalNode {
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
    // let orders: Vec< = orders.into_iter().map(

    // ).collect();

    unimplemented!()
}

fn eval_orders_from_sliding_windows(mir: &RF::RtLolaMir) -> Vec<Vec<EvalNode>> {
    unimplemented!()
}

fn eval_orders_from_outputs(mir: &RF::RtLolaMir) -> Vec<Vec<EvalNode>> {
    unimplemented!()
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
