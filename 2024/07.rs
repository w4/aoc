use std::{io::Read, str::FromStr};

use arrayvec::ArrayVec;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let input = input
        .lines()
        .map(|v| {
            let (l, r) = v.split_once(": ").unwrap();

            (
                u64::from_str(l).unwrap(),
                r.split_whitespace()
                    .map(u16::from_str)
                    .collect::<Result<ArrayVec<u16, 16>, _>>()
                    .unwrap(),
            )
        })
        .collect::<ArrayVec<_, 1024>>();

    play(&input, false);
    play(&input, true);
}

fn play(input: &ArrayVec<(u64, ArrayVec<u16, 16>), 1024>, part2: bool) {
    let mut res = 0;

    for (total, numbers) in input {
        let mut table = vec![u64::from(numbers[0])];

        for number in numbers.into_iter().skip(1) {
            let mut next_table = Vec::new();

            for val in table {
                let add = val + u64::from(*number);
                next_table.push(add);

                let mul = val * u64::from(*number);
                next_table.push(mul);

                if part2 {
                    let con = (val * 10_u64.pow(number.ilog10() + 1)) + u64::from(*number);
                    next_table.push(con);
                }
            }

            table = next_table;
        }

        if table.contains(total) {
            res += total;
        }
    }

    eprintln!("{res:?}");
}
