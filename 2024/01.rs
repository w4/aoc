#![feature(binary_heap_into_iter_sorted)]
#![allow(clippy::cast_possible_truncation)]
use std::{cmp::Reverse, collections::BinaryHeap, io::Read};

use itertools::Itertools;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let (left, right) = parse_input(&input);
    part1(left.clone(), right.clone());
    part2(left.clone(), right.clone());
}

fn part1(left: BinaryHeap<Reverse<u64>>, right: BinaryHeap<Reverse<u64>>) {
    let res: u64 = left
        .into_iter_sorted()
        .zip(right.into_iter_sorted())
        .map(|(Reverse(l), Reverse(r))| l.abs_diff(r))
        .sum();
    eprintln!("part 1: {res}");
}

fn part2(left: BinaryHeap<Reverse<u64>>, right: BinaryHeap<Reverse<u64>>) {
    let counts = right.into_iter().counts();
    let res: usize = left
        .into_iter()
        .map(|v| v.0 as usize * counts.get(&v).copied().unwrap_or_default())
        .sum();
    eprintln!("part 2: {res}");
}

fn parse_input(input: &str) -> (BinaryHeap<Reverse<u64>>, BinaryHeap<Reverse<u64>>) {
    input.split('\n').filter_map(|v| v.split_once("   ")).fold(
        (BinaryHeap::new(), BinaryHeap::new()),
        |(mut left_acc, mut right_acc), (left, right)| {
            left_acc.push(Reverse(left.parse().unwrap()));
            right_acc.push(Reverse(right.parse().unwrap()));
            (left_acc, right_acc)
        },
    )
}
