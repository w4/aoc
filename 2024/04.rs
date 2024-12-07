#![allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]

use std::io::Read;

use itertools::Itertools;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let input = input.trim();

    let rows = input.lines().count();
    let cols = input.split_once('\n').unwrap().0.len();

    let input = input.replace('\n', "");

    part1(&input, rows, cols);
    part2(&input, cols);
}

fn part1(input: &str, rows: usize, cols: usize) {
    const VARIANTS: &[[(isize, isize); 3]] = &[
        // forward
        [(1, 0), (2, 0), (3, 0)],
        // backwards
        [(-1, 0), (-2, 0), (-3, 0)],
        // downwards
        [(0, 1), (0, 2), (0, 3)],
        // upwards
        [(0, -1), (0, -2), (0, -3)],
        // diag up left
        [(-1, -1), (-2, -2), (-3, -3)],
        // diag up right
        [(1, -1), (2, -2), (3, -3)],
        // diag down right
        [(1, 1), (2, 2), (3, 3)],
        // diag down left
        [(-1, 1), (-2, 2), (-3, 3)],
    ];

    let char_at_pos = |n| &input[n..=n];
    let mut found = 0;

    let indicies = input
        .char_indices()
        .filter(|(_, c)| *c == 'X')
        .collect_vec();

    for (pos, _) in indicies {
        let curr_char_row = pos / cols;

        let get_at_offset = |&(x, y): &(isize, isize)| {
            char_at_pos((pos as isize + (cols as isize * y) + x) as usize)
        };

        for [v1, v2, v3] in VARIANTS {
            let (max_x, max_y) = v3;

            // if max_x will overflow the row, or if the max_y will take us outside of the grid,
            // skip
            if (pos as isize + max_x) / cols as isize != curr_char_row as isize
                || curr_char_row as isize + max_y < 0
                || curr_char_row as isize + max_y >= rows as isize
            {
                continue;
            }

            if get_at_offset(v1) == "M" && get_at_offset(v2) == "A" && get_at_offset(v3) == "S" {
                found += 1;
            }
        }
    }

    eprintln!("{found}");
}

fn part2(input: &str, cols: usize) {
    let char_at_pos = |n| &input[n..=n];
    let mut found = 0;

    let indicies = input
        .char_indices()
        .filter(|(_, c)| *c == 'A')
        .collect_vec();

    for (pos, _) in indicies {
        let get_at_offset =
            |(x, y): (isize, isize)| char_at_pos((pos as isize + (cols as isize * y) + x) as usize);

        if pos < cols || pos % cols < 1 || pos + cols > input.len() {
            continue;
        }

        let top_left = get_at_offset((-1, -1));
        let top_right = get_at_offset((1, -1));
        let bottom_left = get_at_offset((-1, 1));
        let bottom_right = get_at_offset((1, 1));

        match (top_left, top_right, bottom_left, bottom_right) {
            ("M", "S", "M", "S")
            | ("S", "M", "S", "M")
            | ("M", "M", "S", "S")
            | ("S", "S", "M", "M") => found += 1,
            _ => {}
        }
    }

    eprintln!("{found}");
}
