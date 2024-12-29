use std::io::Read;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    let input = parse_input(&input);
    part1(&input);
}

fn part1(input: &Input) {
    let mut out = 0;

    for &(a, b, c, d, e) in &input.keys {
        for a in 0..(6 - a) {
            for b in 0..(6 - b) {
                for c in 0..(6 - c) {
                    for d in 0..(6 - d) {
                        for e in 0..(6 - e) {
                            if input.locks[a][b][c][d][e] {
                                out += 1;
                            }
                        }
                    }
                }
            }
        }
    }

    eprintln!("{out}");
}

#[derive(Default, Debug)]
struct Input {
    keys: Vec<(usize, usize, usize, usize, usize)>,
    locks: [[[[[bool; 7]; 7]; 7]; 7]; 7],
}

#[allow(clippy::many_single_char_names)]
fn parse_input(s: &str) -> Input {
    let mut input = Input::default();

    for combo in s.split("\n\n") {
        let matrix = combo
            .split('\n')
            .map(|v| v.chars().collect::<Vec<_>>())
            .collect::<Vec<_>>();

        let count_for = |col| (0..7).filter(|&y| matrix[y][col] == '#').count() - 1;

        let a = count_for(0);
        let b = count_for(1);
        let c = count_for(2);
        let d = count_for(3);
        let e = count_for(4);

        if matrix[0].contains(&'#') {
            input.locks[a][b][c][d][e] = true;
        } else {
            input.keys.push((a, b, c, d, e));
        }
    }

    input
}
