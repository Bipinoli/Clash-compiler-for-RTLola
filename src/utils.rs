use std::collections::HashMap;

fn extract_roots(orders: Vec<Vec<usize>>) -> (Vec<usize>, Vec<Vec<usize>>) {
    let mut roots: HashMap<usize, bool> = HashMap::new();
    for order in &orders {
        if let Some(first_elem) = order.first() {
            roots.entry(*first_elem).or_insert(true);
        }
        for elem in &order[1..] {
            roots.insert(*elem, false);
        }
    }
    let mut actual_roots: Vec<usize> = roots
        .iter()
        .filter_map(|(&elem, &is_root)| if is_root { Some(elem) } else { None })
        .collect();
    let filtered: Vec<Vec<usize>> = orders
        .into_iter()
        .map(|order| order.into_iter().filter(|elem| !roots[elem]).collect())
        .collect();
    actual_roots.sort();
    (actual_roots, filtered)
}

pub fn merge_orders(orders: Vec<Vec<usize>>) -> Vec<Vec<usize>> {
    let orders: Vec<Vec<usize>> = orders.into_iter().filter(|lst| !lst.is_empty()).collect();
    if orders.is_empty() {
        return vec![];
    }
    let (roots, rest) = extract_roots(orders);
    assert!(!roots.is_empty(), "cycle detected");
    let mut merged_order = vec![roots];
    merged_order.extend(merge_orders(rest));
    merged_order
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_roots_works() {
        let inpt_lst = vec![vec![0, 1, 2], vec![3, 5], vec![2, 4]];
        let expected_root = vec![0, 3];
        let expected_rest = vec![vec![1, 2], vec![5], vec![2, 4]];
        let (root, rest) = extract_roots(inpt_lst);
        assert_eq!(root, expected_root);
        assert_eq!(rest, expected_rest);
    }

    #[test]
    fn merge_orders_works() {
        let inpt_lst = vec![
            vec![2, 3, 4],
            vec![2, 1],
            vec![8, 7],
            vec![1, 8, 5],
            vec![1, 6],
            vec![2, 5],
            vec![2, 8, 5],
        ];
        let expected = vec![vec![2], vec![1, 3], vec![4, 6, 8], vec![5, 7]];
        let actual = merge_orders(inpt_lst);
        assert_eq!(expected, actual);
    }
}
