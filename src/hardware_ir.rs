use std::collections::{HashMap, HashSet};

use rtlola_frontend::mir::{Offset, RtLolaMir, StreamAccessKind, StreamReference};

use crate::utils;

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord)]
enum EvalNode {
    InputWindow(usize),
    OutputStream(usize),
    SlidingWindow(usize),
}

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord)]
pub struct OffsetNode {
    from: Box<Node>,
    by: usize,
}

#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord)]
pub enum Node {
    InputStream(usize),
    Offset(OffsetNode),
    OutputStream(usize),
    SlidingWindow(usize),
}

#[derive(PartialEq, Clone)]
struct HardwareIR {
    mir: RtLolaMir,
    evaluation_order: Vec<Vec<Node>>,
}

impl HardwareIR {
    fn new(mir: RtLolaMir) -> Self {
        HardwareIR {
            mir: mir,
            evaluation_order: vec![],
        }
    }

    fn extract_roots() {
        // There can be a sub-graph which generates periodc signals without consuming the inputs
        // Hence we can't reach all the nodes just by traversing from the inputs
        unimplemented!()
    }

    fn build_eval_tree() {
        // Starting from the roots perform breadth first traversal
        // Making sure to avoid infinitely looping on the cycle by tracking the visited nodes
        // However, naive BFS won't work as there could be multiple ways to reach the same descendent node
        // In such cases, we must choose the longest way to reach a descendent node due to the data dependency
        // For this, we order nodes s.t the ones that can reach the others under consideration must be evaluated first
        unimplemented!()
    }

    fn mark_non_pipeline() {
        // Cycle in RTLola can only exist along the offset edge
        // For each node that consumes data from offset
        // Check the distance (d) along the eval tree to reach such source
        // If we can reach then the cycle exists
        // If the distance > offset value, then the data transfer takes too long
        // Which means the current node can't be evaluated in a pipeline fashion
        // Due to the data dependency all of the children & parents of a node that can't be pipelined can't be pipelined as well
        unimplemented!()
    }


}


pub fn node_tree(mir: RtLolaMir) -> Vec<Vec<Node>> {
    let mut orders = node_orders_from_sliding_windows(&mir);
    let ouput_orders = node_orders_from_outputs(&mir);
    orders.extend(ouput_orders);
    merge_nodes_into_tree(orders)
}

fn merge_nodes_into_tree(orders: Vec<Vec<Node>>) -> Vec<Vec<Node>> {
    let mut set = HashSet::new();
    for order in orders.clone() {
        for node in order {
            set.insert(node);
        }
    }
    let mut keys: Vec<Node> = set.into_iter().collect();
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
    let merged_orders: Vec<Vec<Node>> = merged_orders
        .into_iter()
        .map(|v| {
            v.into_iter()
                .map(|i| reverse_map.get(&i).unwrap().clone())
                .collect()
        })
        .collect();
    merged_orders
}

fn node_orders_from_sliding_windows(mir: &RtLolaMir) -> Vec<Vec<Node>> {
    let mut orders: Vec<Vec<Node>> = vec![];
    for sw in &mir.sliding_windows {
        // target -> window -> caller
        let target = match sw.target {
            StreamReference::In(x) => Node::InputStream(x),
            StreamReference::Out(x) => Node::OutputStream(x),
        };
        let window = Node::SlidingWindow(sw.reference.idx());
        let caller = match sw.caller {
            StreamReference::In(x) => Node::InputStream(x),
            StreamReference::Out(x) => Node::OutputStream(x),
        };
        orders.push(vec![target, window, caller]);
    }
    orders
}

fn node_orders_from_outputs(mir: &RtLolaMir) -> Vec<Vec<Node>> {
    let mut orders: Vec<Vec<Node>> = vec![];
    for out in &mir.outputs {
        for (acced_ref, info) in &out.accesses {
            let (_, info) = info.first().unwrap();
            let offset: Option<u32> = match info {
                StreamAccessKind::Offset(x) => match x {
                    Offset::Past(p) => Some(p.clone()),
                    _ => None,
                },
                _ => None,
            };
            let accessed_node = match offset {
                Some(off) => match acced_ref {
                    StreamReference::In(x) => Node::Offset(OffsetNode {
                        from: Box::new(Node::InputStream(x.clone())),
                        by: off.clone() as usize,
                    }),
                    StreamReference::Out(x) => Node::Offset(OffsetNode {
                        from: Box::new(Node::OutputStream(x.clone())),
                        by: off.clone() as usize,
                    }),
                },
                None => match acced_ref {
                    StreamReference::In(x) => Node::InputStream(x.clone()),
                    StreamReference::Out(x) => Node::OutputStream(x.clone()),
                },
            };
            orders.push(vec![
                accessed_node,
                Node::OutputStream(out.reference.out_ix()),
            ]);
        }
    }
    orders
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge_nodes_into_tree_works() {
        // from sliding window analysis
        let mut orders = vec![
            vec![
                Node::OutputStream(1),
                Node::SlidingWindow(0),
                Node::OutputStream(2),
            ],
            vec![
                Node::OutputStream(2),
                Node::SlidingWindow(2),
                Node::OutputStream(4),
            ],
            vec![
                Node::OutputStream(0),
                Node::SlidingWindow(1),
                Node::OutputStream(3),
            ],
        ];
        // from outputs analysis
        let output_orders = vec![
            vec![
                Node::Offset(OffsetNode {
                    from: Box::new(Node::InputStream(0)),
                    by: 1,
                }),
                Node::OutputStream(0),
            ],
            vec![Node::OutputStream(0), Node::OutputStream(1)],
            vec![Node::SlidingWindow(0), Node::OutputStream(2)],
            vec![Node::SlidingWindow(1), Node::OutputStream(3)],
            vec![Node::SlidingWindow(2), Node::OutputStream(4)],
        ];
        orders.extend(output_orders);

        let expected = vec![
            vec![Node::Offset(OffsetNode {
                from: Box::new(Node::InputStream(0)),
                by: 1,
            })],
            vec![Node::OutputStream(0)],
            vec![Node::OutputStream(1), Node::SlidingWindow(1)],
            vec![Node::OutputStream(3), Node::SlidingWindow(0)],
            vec![Node::OutputStream(2)],
            vec![Node::SlidingWindow(2)],
            vec![Node::OutputStream(4)],
        ];

        let actual = merge_nodes_into_tree(orders);
        assert_eq!(actual, expected);
    }
}
