use crate::analysis::node::Node;

use rtlola_frontend::mir::RtLolaMir;
use std::{collections::HashSet, usize, vec};

use crate::analysis::pipeline;

pub fn merge_eval_orders_various_combinations(
    orders: Vec<Vec<Vec<Node>>>,
    mir: &RtLolaMir,
) -> Vec<Vec<Node>> {
    // For example:
    // ```
    // input x: Int
    // output a := x + 1
    // output b := a + 1
    // output c := b + 1
    // output d := c.offset(by: -1).defaults(to: 0)
    // ```
    //
    // - After removing offsets the isolated graphs with eval orders are `[x, a, b, c]` and `[d]`
    // - We could try all combinations of eval orders i.e:
    //     - `[(x, d), a, b, c]`
    //     - `[x, (a, d), b, c]`
    //     - `[x, a, (b, d), c]`
    //     - `[x, a, b, (c, d)]`
    //     - `[x, a, b, c, d]`
    // - Among them the eval order `[x, a, b, (c, d)]` & `[x, a, b, c, d]` will give the least `pipeline_wait` necessary i.e `0`
    // - However `[x, a, b, (c, d)]` is more desirable as the total eval layers are smaller and we get to evaluate d as early as possible
    // - When we look deeper, it makes sense as when evaluating `c` we will already have the past value of `c` availabe
    // - Whereas if we had tried to evaluate `d` along with `b` then we would need to wait one cycle until the `c` gets evaluated
    //
    // - Time complexity: `O(n ^ (k + 1))`
    // - where `n` = max length of eval order ~ number of nodes
    //     - `k` = number of disjointed sub-graphs
    //     - here we have `n` choice to start `k` eval-orders and each pipeline_wait calculation takes `O(n)` hence `k + 1` in the exponent

    if orders.is_empty() {
        return Vec::new();
    }
    if orders.len() == 1 {
        return orders.first().unwrap().to_vec();
    }
    let mut orders = orders.clone();
    orders.sort_by(|a, b| b.len().cmp(&a.len()));
    let n = orders.first().unwrap().len();
    let all_starts = gen_all_combinations(n, orders.len() - 1);
    let all_combinations: Vec<Vec<Vec<Node>>> = all_starts
        .into_iter()
        .map(|start| merge_offsetted_eval_orders(&orders, start))
        .collect::<HashSet<_>>()
        .into_iter()
        .collect();
    all_combinations
        .into_iter()
        .reduce(|best, item| choose_better_eval_order(best, item, mir))
        .unwrap()
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

fn merge_offsetted_eval_orders(
    eval_orders: &Vec<Vec<Vec<Node>>>,
    starts: Vec<usize>,
) -> Vec<Vec<Node>> {
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

fn choose_better_eval_order(
    eval_order1: Vec<Vec<Node>>,
    eval_order2: Vec<Vec<Node>>,
    mir: &RtLolaMir,
) -> Vec<Vec<Node>> {
    let pipeline_wait1 = pipeline::calculate_necessary_pipeline_wait(&eval_order1, mir);
    let pipeline_wait2 = pipeline::calculate_necessary_pipeline_wait(&eval_order2, mir);
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
    for i in 0..(std::cmp::min(eval_order1.len(), eval_order2.len())) {
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
