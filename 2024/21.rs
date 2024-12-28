use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

use arrayvec::ArrayVec;
use itertools::Itertools;

fn main() -> anyhow::Result<()> {
    let input = std::io::stdin()
        .lines()
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .filter(|v| !v.is_empty())
        .map(|v| map_input(&v))
        .collect_vec();

    let mut part1 = 0;
    let mut part2 = 0;

    for input in input {
        let code = input
            .iter()
            .filter(|v| **v != 10)
            .fold(0, |acc, curr| acc * 10 + (*curr as usize));

        part1 += code * solve(input, 2);
        part2 += code * solve(input, 25);
    }

    eprintln!("{part1}");
    eprintln!("{part2}");

    Ok(())
}

fn solve(input: [u8; 5], directional_robots: u8) -> usize {
    let mut out = 0;

    for (&a, &b) in input.iter().tuple_windows() {
        let buttons = inputs_for_keypad(a, b);
        let mut cache = HashMap::new();

        out += expand_inputs(&buttons, directional_robots, directional_robots, &mut cache);
    }

    out
}

fn hash_buttons(v: &[Button]) -> u64 {
    let mut hasher = std::hash::DefaultHasher::new();
    v.hash(&mut hasher);
    hasher.finish()
}

fn expand_inputs(
    buttons: &[Button],
    n: u8,
    max_n: u8,
    cache: &mut HashMap<(u64, u8), usize>,
) -> usize {
    let button_hash = hash_buttons(buttons);
    if let Some(cached) = cache.get(&(button_hash, n)) {
        return *cached;
    }

    if n == 0 {
        cache.insert((button_hash, n), buttons.len());
        return buttons.len();
    }

    let mut acc = 0;

    for (&a, &b) in (n != max_n)
        .then_some(&Button::A)
        .into_iter()
        .chain(buttons.iter())
        .tuple_windows()
    {
        acc += expand_inputs(&inputs_for_robot(a, b), n - 1, max_n, cache);
    }

    cache.insert((button_hash, n), acc);

    acc
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Button {
    Up,
    Down,
    Left,
    Right,
    A,
}

fn inputs_for_robot(a: Button, b: Button) -> ArrayVec<Button, 4> {
    let mut out = ArrayVec::new();

    match (a, b) {
        (Button::A, Button::Up) | (Button::Right, Button::Down) | (Button::Down, Button::Left) => {
            out.push(Button::Left);
        }
        (Button::A, Button::Right) => {
            out.push(Button::Down);
        }
        (Button::A, Button::Down) => {
            out.push(Button::Left);
            out.push(Button::Down);
        }
        (Button::A, Button::Left) => {
            out.push(Button::Down);
            out.push(Button::Left);
            out.push(Button::Left);
        }
        (Button::Up, Button::A) | (Button::Left, Button::Down) | (Button::Down, Button::Right) => {
            out.push(Button::Right);
        }
        (Button::Right, Button::A) => {
            out.push(Button::Up);
        }
        (Button::Down, Button::A) => {
            out.push(Button::Up);
            out.push(Button::Right);
        }
        (Button::Left, Button::A) => {
            out.push(Button::Right);
            out.push(Button::Right);
            out.push(Button::Up);
        }
        (Button::Left, Button::Up) => {
            out.push(Button::Right);
            out.push(Button::Up);
        }
        (Button::Up, Button::Left) => {
            out.push(Button::Down);
            out.push(Button::Left);
        }
        (Button::Right, Button::Up) => {
            out.push(Button::Left);
            out.push(Button::Up);
        }
        (Button::Up, Button::Right) => {
            out.push(Button::Down);
            out.push(Button::Right);
        }
        _ => {}
    }

    out.push(Button::A);

    out
}

fn inputs_for_keypad(a: u8, b: u8) -> Vec<Button> {
    // bits = 0b1111 = up | down | left | right
    const ALLOWED_DIRECTIONS: [u8; 11] = [
        0b1001, // 0
        0b1001, // 1
        0b1111, // 2
        0b1110, // 3
        0b1101, // 4
        0b1111, // 5
        0b1110, // 6
        0b0101, // 7
        0b0111, // 8
        0b0110, // 9
        0b1010, // A
    ];

    const POSITIONS: [(u8, u8); 11] = [
        (1, 3), // 0
        (0, 2), // 1
        (1, 2), // 2
        (2, 2), // 3
        (0, 1), // 4
        (1, 1), // 5
        (2, 1), // 6
        (0, 0), // 7
        (1, 0), // 8
        (2, 0), // 9
        (2, 3), // A
    ];

    const NUMBERS: [[u8; 3]; 4] = [[7, 8, 9], [4, 5, 6], [1, 2, 3], [255, 0, 10]];

    assert!(a <= 10);
    assert!(b <= 10);

    let (mut cx, mut cy) = POSITIONS[a as usize];
    let (dx, dy) = POSITIONS[b as usize];
    let up_preferred = cy == 3;
    let mut out = Vec::new();

    out.push(Button::A);

    while (cx, cy) != (dx, dy) {
        let c = NUMBERS[cy as usize][cx as usize];
        let allowed_directions = ALLOWED_DIRECTIONS[c as usize];

        if up_preferred && cy > dy && b != 2 {
            cy -= 1;
            out.push(Button::Up);
        } else if cx > dx && allowed_directions & 0b0010 != 0 {
            cx -= 1;
            out.push(Button::Left);
        } else if cy < dy && allowed_directions & 0b0100 != 0 {
            cy += 1;
            out.push(Button::Down);
        } else if cy > dy && allowed_directions & 0b1000 != 0 {
            cy -= 1;
            out.push(Button::Up);
        } else if cx < dx && allowed_directions & 0b0001 != 0 {
            cx += 1;
            out.push(Button::Right);
        }
    }

    out.push(Button::A);

    out
}

fn map_input(s: &str) -> [u8; 5] {
    let chars = s.chars().collect_vec();
    assert_eq!(chars.len(), 4);

    let map_char = |c| match c {
        '0' => 0,
        '1' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
        'A' => 10,
        _ => panic!("invalid char"),
    };

    [
        10, // always starts at A
        map_char(chars[0]),
        map_char(chars[1]),
        map_char(chars[2]),
        map_char(chars[3]),
    ]
}
