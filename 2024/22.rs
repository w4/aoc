#![allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
use std::str::FromStr;

fn main() -> anyhow::Result<()> {
    let input = std::io::stdin()
        .lines()
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(|v| u32::from_str(&v))
        .collect::<Result<Vec<_>, _>>()?;

    println!("{}", part1(&input));
    println!("{}", part2(&input));

    Ok(())
}

fn part1(input: &[u32]) -> u64 {
    let mut acc = 0;

    for &i in input {
        let mut c = i;

        for _ in 0..2000 {
            c = cipher(c);
        }

        acc += u64::from(c);
    }

    acc
}

fn part2(input: &[u32]) -> u32 {
    const RANGE: usize = 19;
    const OFFSET: usize = 9;
    let mut tally =
        vec![vec![[[0u32; RANGE]; RANGE]; RANGE].into_boxed_slice(); RANGE].into_boxed_slice();
    let mut seen =
        vec![vec![[[0; RANGE]; RANGE]; RANGE].into_boxed_slice(); RANGE].into_boxed_slice();
    let mut generation = 1;

    for &i in input {
        let mut prev = cipher(i);
        let mut a = prev;
        let mut b = cipher(a);
        let mut c = cipher(b);
        let mut d = cipher(c);

        for _ in 0..1998_u32 {
            let dp = (prev % 10) as i8;
            let da = (a % 10) as i8;
            let db = (b % 10) as i8;
            let dc = (c % 10) as i8;
            let dd = (d % 10) as i8;

            let delta1 = (da - dp + OFFSET as i8) as usize;
            let delta2 = (db - da + OFFSET as i8) as usize;
            let delta3 = (dc - db + OFFSET as i8) as usize;
            let delta4 = (dd - dc + OFFSET as i8) as usize;

            if seen[delta1][delta2][delta3][delta4] != generation {
                seen[delta1][delta2][delta3][delta4] = generation;
                tally[delta1][delta2][delta3][delta4] += d % 10;
            }

            prev = a;
            a = b;
            b = c;
            c = d;
            d = cipher(d);
        }

        generation += 1;
    }

    tally
        .iter()
        .flatten()
        .flatten()
        .flatten()
        .max()
        .copied()
        .unwrap_or(u32::MAX)
}

fn cipher(n: u32) -> u32 {
    let n = ((n << 6) ^ n) & 0x00FF_FFFF;
    let n = ((n >> 5) ^ n) & 0x00FF_FFFF;
    ((n << 11) ^ n) & 0x00FF_FFFF
}
