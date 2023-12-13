use std::{
    fs::File,
    io::{Read, Write},
    iter::once,
    str::FromStr,
    sync::{Arc, Condvar, Mutex},
};

use arrayvec::ArrayVec;
use nom::{
    branch::alt,
    character::complete::{char, digit1},
    combinator::{map, map_res},
    multi::{many1, separated_list1},
    sequence::separated_pair,
    IResult,
};

fn main() {
    let mut input = Vec::new();
    std::io::stdin().lock().read_to_end(&mut input).unwrap();
    let input = std::str::from_utf8(&input).unwrap();

    let (rest, input) = parse_input(input).unwrap();
    assert!(rest.is_empty());

    part1(input.clone());
    part2(input.clone());
}

fn part1(input: Vec<RawInput>) {
    let lines = input.into_iter().map(Line::from).collect::<Vec<_>>();

    let res = run(lines, "p1");
    eprintln!("part 1: {res}");
}

fn part2(input: Vec<RawInput>) {
    let lines = input
        .into_iter()
        .map(|mut line| {
            line.kinds = (0..5)
                .flat_map(|_| line.kinds.iter().copied().chain(once(Kind::Unknown)))
                .collect();
            line.kinds.remove(line.kinds.len() - 1);

            line.damaged = line.damaged.repeat(5);
            line
        })
        .map(Line::from)
        .collect::<Vec<_>>();

    eprintln!("part 2: {}", run(lines, "p2-test"));
}

fn run(line: Vec<Line>, suffix: &str) -> u64 {
    let mut out_file = File::options()
        .read(true)
        .create(true)
        .append(true)
        .open(format!("out-{suffix}"))
        .unwrap();

    let mut seen = String::new();
    out_file.read_to_string(&mut seen).unwrap();

    let seen = seen
        .lines()
        .filter_map(|v| v.split_once(','))
        .map(|(idx, _)| usize::from_str(idx).unwrap())
        .collect::<Vec<_>>();
    eprintln!("recovered state - completed {seen:?}");

    let (completed_send, completed_recv) = std::sync::mpsc::channel();
    let handle = std::thread::spawn(move || {
        while let Ok((i, c)) = completed_recv.recv() {
            writeln!(out_file, "{i},{c}").unwrap();
        }

        eprintln!("shutdown");
    });

    let mut processing_handles = vec![];

    let parallelism = std::thread::available_parallelism().unwrap().get();
    eprintln!(
        "using {parallelism} threads to process {}/{} tasks",
        line.len() - seen.len(),
        line.len()
    );

    let completed_count = Arc::new((Mutex::new(0_usize), Condvar::new()));

    for (i, mut line) in line.into_iter().enumerate() {
        if seen.contains(&i) {
            continue;
        }

        {
            let mut val = completed_count
                .1
                .wait_while(completed_count.0.lock().unwrap(), |count| {
                    *count >= parallelism
                })
                .unwrap();
            *val += 1;
        }

        let completed_send = completed_send.clone();
        let completed_count = completed_count.clone();
        processing_handles.push(std::thread::spawn(move || {
            let damaged_length: u32 = line.damaged.iter().map(|(_, v)| v).sum();
            let output_length = 128 - (line.kinds | line.unknown).leading_zeros();

            line.damaged.reverse();

            eprintln!("processing {i}");

            let res = process_new(
                ConstInput {
                    input: line.kinds,
                    output_length: output_length - damaged_length + 2
                        - u32::try_from(line.damaged.len()).unwrap(),
                    expected_packed: line.damaged_packed,
                    unwritable_area: !(line.kinds | line.unknown),
                },
                &line.damaged,
                0,
                0,
                0,
                0,
            );

            *completed_count.0.lock().unwrap() -= 1;
            completed_count.1.notify_one();

            completed_send.send((i, res)).unwrap();

            eprintln!("{i} - {res}");

            res
        }));
    }

    drop(completed_send);

    let mut acc = 0;
    for h in processing_handles {
        acc += h.join().unwrap();
    }

    handle.join().unwrap();

    acc
}

#[derive(Copy, Clone)]
pub struct ConstInput {
    input: u128,
    output_length: u32,
    expected_packed: u128,
    unwritable_area: u128,
}

/// Rather than bruteforcing the inputs against the outputs, we'll instead turn
/// this into a bit-fitting issue on the outputs - we know the groups of 1s we
/// need to fit (ie. `3, 2, 1`) we'll turn that expected output into `11101101`
/// and see how many times we can increase the amount of zeros between the groups
/// and have it not write outside of `(input | unknown)`.
fn process_new(
    c: ConstInput,
    expected_list: &[(u128, u32)],
    current: u128,
    i: u32,
    shift: u32,
    mask: u32,
) -> u64 {
    // take the next group out of the list
    let (mut expected_as_bits, current_length) = expected_list[0];
    let expected_rest = &expected_list[1..];

    // the group as bits, so if the input asks for a group of `3` operational
    // gears, we'll have `111` in this, shifted by the previous groups and
    // their zeros
    expected_as_bits <<= shift;

    let mut acc = 0_u64;

    if expected_rest.is_empty() {
        for zeros in 0_u32..(c.output_length - i) {
            // write our new bits with offsetted zeros to the current value
            let with_new_bits = c.input | current | (expected_as_bits << zeros);

            if with_new_bits & c.unwritable_area == 0
                && compress_zeros(with_new_bits) == c.expected_packed
            {
                acc += 1;
            }
        }
    } else {
        // the base shift we'll ask the next group to do, which is the shift we
        // were asked to do, along with the length of our number, plus our reserved
        // 0.
        let next_shift = shift + current_length + 1;
        let mask = mask + current_length;
        let m = (1 << mask) - 1;

        // a mask over the compressed zeros we will be writing to so we can chop
        // branches if our current write doesn't match the bit pattern expected
        let c_exp = c.expected_packed & m;

        for zeros in 0_u32..(c.output_length - i) {
            // push our group into the number followed by `zeros` zeros.
            let with_new_bits = current | (expected_as_bits << zeros);

            if with_new_bits & c.unwritable_area == 0
                && (compress_zeros(c.input | with_new_bits) & m) == c_exp
            {
                // give the next group `zeros` less zeros along with our reserved 0
                acc += process_new(
                    c,
                    expected_rest,
                    with_new_bits,
                    i + zeros,
                    next_shift + zeros,
                    mask + 1,
                );
            }
        }
    }

    acc
}

/// Takes an input such as `11110001111001` and compresses successive zeros
/// down to a single zero (ie. `11110111101`). This will let us compare our
/// current solution (`input | current`) over the expected output.
#[inline]
fn compress_zeros(mut num: u128) -> u128 {
    let mut result: u128 = 0;
    let mut last_bit = true;
    let mut idx: u8 = 0;

    num >>= num.trailing_zeros();

    while num > 0 {
        let current_bit = num & 1;
        let cbit_bool = current_bit != 0;

        if cbit_bool {
            result |= 1 << idx;
            idx += 1;
            num >>= 1;
            last_bit = true;
        } else if last_bit {
            idx += 1;
            last_bit = false;
            num >>= num.trailing_zeros();
        }
    }

    result
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Kind {
    Damaged,
    Operational,
    Unknown,
}

#[derive(Debug, Clone)]
pub struct Line {
    kinds: u128,
    unknown: u128,
    damaged_packed: u128,
    damaged: ArrayVec<(u128, u32), 32>,
}

impl From<RawInput> for Line {
    fn from(
        RawInput {
            kinds,
            damaged: damaged_vec,
        }: RawInput,
    ) -> Self {
        let (kinds, unknown) = kinds.into_iter().rev().enumerate().fold(
            (0, 0),
            |(kinds_acc, unknown_acc), (i, kind)| match kind {
                Kind::Damaged => (kinds_acc | (1 << i), unknown_acc),
                Kind::Operational => (kinds_acc, unknown_acc),
                Kind::Unknown => (kinds_acc, unknown_acc | (1 << i)),
            },
        );

        let mut damaged_packed = 0_u128;
        for d in damaged_vec.clone() {
            damaged_packed <<= d + 1;
            damaged_packed |= (1 << d) - 1;
        }

        let damaged = damaged_vec
            .into_iter()
            .map(|v| ((1_u128 << v) - 1, ((1_u32 << v) - 1).trailing_ones()))
            .collect();

        Self {
            kinds,
            unknown,
            damaged_packed,
            damaged,
        }
    }
}

#[derive(Clone, Debug)]
pub struct RawInput {
    kinds: Vec<Kind>,
    damaged: Vec<u8>,
}

impl From<(Vec<Kind>, Vec<u8>)> for RawInput {
    fn from((kinds, damaged): (Vec<Kind>, Vec<u8>)) -> Self {
        Self { kinds, damaged }
    }
}

fn parse_input(rest: &str) -> IResult<&str, Vec<RawInput>> {
    separated_list1(char('\n'), parse_line)(rest)
}

fn parse_line(rest: &str) -> IResult<&str, RawInput> {
    map(
        separated_pair(fold_chars, char(' '), parse_damaged),
        RawInput::from,
    )(rest)
}

fn fold_chars(rest: &str) -> IResult<&str, Vec<Kind>> {
    many1(parse_char)(rest)
}

fn parse_char(rest: &str) -> IResult<&str, Kind> {
    alt((
        map(char('#'), |_| Kind::Damaged),
        map(char('.'), |_| Kind::Operational),
        map(char('?'), |_| Kind::Unknown),
    ))(rest)
}

fn parse_damaged(rest: &str) -> IResult<&str, Vec<u8>> {
    separated_list1(char(','), map_res(digit1, u8::from_str))(rest)
}
