use std::collections::{HashMap, HashSet};

#[allow(clippy::cast_possible_wrap)]
fn main() {
    let mut frequencies = HashMap::<char, Vec<(i64, i64)>>::new();
    let mut max_x = 0;
    let mut max_y = 0;

    for (y, line) in std::io::stdin().lines().enumerate() {
        for (x, c) in line.unwrap().chars().enumerate() {
            if c != '.' {
                frequencies.entry(c).or_default().push((x as i64, y as i64));
            }
            max_x = x as i64;
        }
        max_y = y as i64;
    }

    let mut part1 = HashSet::new();
    let mut part2 = HashSet::new();

    for frequencies in frequencies.values() {
        for (x1, y1) in frequencies {
            for (x2, y2) in frequencies {
                if x1 == x2 && y1 == y2 {
                    continue;
                }

                let vx = x2 - x1;
                let vy = y2 - y1;

                let derive = |out: &mut HashSet<_>, n| {
                    let mut hit = false;

                    let dx = x1 + n * vx;
                    let dy = y1 + n * vy;
                    if dx >= 0 && dx <= max_x && dy >= 0 && dy <= max_y {
                        out.insert((dx, dy));
                        hit = true;
                    }

                    let dx = x2 - n * vx;
                    let dy = y2 - n * vy;
                    if dx >= 0 && dx <= max_x && dy >= 0 && dy <= max_y {
                        out.insert((dx, dy));
                        hit = true;
                    }

                    hit
                };

                derive(&mut part1, 2);

                for i in 0.. {
                    if !derive(&mut part2, i) {
                        break;
                    }
                }
            }
        }
    }

    eprintln!("{}", part1.len());
    eprintln!("{}", part2.len());
}
