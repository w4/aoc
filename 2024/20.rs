#![allow(clippy::cast_possible_wrap)]
use std::{
    collections::{HashSet, VecDeque},
    io::Read,
};

use indexmap::IndexMap;
use itertools::Itertools;

fn main() -> anyhow::Result<()> {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input)?;

    let input = parse_input(&input);

    let distances = dists(&input);

    part1(&distances);
    part2(&distances);

    Ok(())
}

fn part1(distances: &IndexMap<(usize, usize), isize>) {
    let mut out = 0;

    for ((node1, d1), (node2, d2)) in distances.iter().tuple_combinations() {
        let dist = (node1.0.abs_diff(node2.0) + node1.1.abs_diff(node2.1)) as isize;

        if dist == 2 && d2 - d1 - dist >= 100 {
            out += 1;
        }
    }

    eprintln!("{out}");
}

fn part2(distances: &IndexMap<(usize, usize), isize>) {
    let mut out = 0;

    for ((node1, d1), (node2, d2)) in distances.iter().tuple_combinations() {
        let dist = (node1.0.abs_diff(node2.0) + node1.1.abs_diff(node2.1)) as isize;

        if dist <= 20 && d2 - d1 - dist >= 100 {
            out += 1;
        }
    }

    eprintln!("{out}");
}

fn dists(input: &Input) -> IndexMap<(usize, usize), isize> {
    let mut queue = VecDeque::new();
    let mut distances = IndexMap::new();

    distances.insert(input.start, 0);
    queue.push_back((input.start, 0));

    while let Some(((nx, ny), dist)) = queue.pop_front() {
        for next in [
            (nx.wrapping_sub(1), ny),
            (nx + 1, ny),
            (nx, ny.wrapping_sub(1)),
            (nx, ny + 1),
        ] {
            if input.grid.contains(&next) && !distances.contains_key(&next) {
                distances.insert(next, dist + 1);
                queue.push_back((next, dist + 1));
            }
        }
    }

    distances
}

#[derive(Default, Debug, Clone)]
pub struct Input {
    start: (usize, usize),
    end: (usize, usize),
    grid: HashSet<(usize, usize)>,
}

fn parse_input(s: &str) -> Input {
    let mut input = Input::default();

    for (y, s) in s.trim().lines().enumerate() {
        for (x, c) in s.chars().enumerate() {
            match c {
                '#' => continue,
                'S' => input.start = (x, y),
                'E' => input.end = (x, y),
                '.' => (),
                _ => panic!("invalid char in input: {c}"),
            }

            input.grid.insert((x, y));
        }
    }

    input
}
