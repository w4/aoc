use std::{collections::HashMap, io::Read, ops::Range, str::FromStr, time::Instant};

use itertools::Itertools;
use nom::IResult;
use rangemap::RangeMap;

const TRANSLATION_PATH: &[MapKind] = &[
    MapKind::Soil,
    MapKind::Fertilizer,
    MapKind::Water,
    MapKind::Light,
    MapKind::Temperature,
    MapKind::Humidity,
    MapKind::Location,
];

fn main() {
    let mut input = Vec::new();
    std::io::stdin().lock().read_to_end(&mut input).unwrap();
    let input = std::str::from_utf8(&input).unwrap();

    let (rest, input) = parse_input(input).unwrap();
    assert!(rest.is_empty());

    let i = Instant::now();
    let answer = part1(&input);
    eprintln!("part 1: {answer} ({:?})", i.elapsed());

    let i = Instant::now();
    let answer = part2(&input);
    eprintln!("part 2: {answer} ({:?})", i.elapsed());
}

fn part1(input: &Input) -> u64 {
    let mut lowest_location = u64::MAX;

    for seed in &input.seeds {
        let mut source = *seed;
        let mut from = MapKind::Seed;

        for to in TRANSLATION_PATH {
            let Some(translation) = input.maps.get(&Translation { from, to: *to }) else {
                panic!("invalid path {from:?} to {to:?}");
            };

            if let Some((source_range, destination_base)) = translation.get_key_value(&source) {
                source = destination_base + (source - source_range.start);
            }

            from = *to;
        }

        assert_eq!(from, MapKind::Location);
        lowest_location = lowest_location.min(source);
    }

    lowest_location
}

fn part2(input: &Input) -> u64 {
    let seed_ranges: Vec<_> = input
        .seeds
        .iter()
        .tuples()
        .map(|(start, len)| (*start)..(*start) + len)
        .collect();

    let mut lowest_bound_seen = u64::MAX;

    for seed_range in seed_ranges {
        let lowest_for_seed = traverse_path(input, TRANSLATION_PATH, MapKind::Seed, seed_range);
        lowest_bound_seen = lowest_bound_seen.min(lowest_for_seed);
    }

    lowest_bound_seen
}

fn traverse_path(input: &Input, path: &[MapKind], from: MapKind, source_range: Range<u64>) -> u64 {
    let mut lowest_bound_seen = u64::MAX;

    let Some((next_path, rest)) = path.split_first() else {
        return source_range.start;
    };

    let Some(translation) = input.maps.get(&Translation {
        from,
        to: *next_path,
    }) else {
        panic!("invalid path {from:?} to {next_path:?}");
    };

    for (new_source_range, destination_base) in translation.overlapping(&source_range) {
        // determine intersection between the source range and destination range
        let start = source_range.start.max(new_source_range.start);
        let end = source_range.end.min(new_source_range.end);
        let offset = start.saturating_sub(new_source_range.start);
        let length = end.saturating_sub(start);

        let destination_range = (*destination_base + offset)..(*destination_base + offset + length);

        let lowest_in_tree = traverse_path(input, rest, *next_path, destination_range);

        lowest_bound_seen = lowest_bound_seen.min(lowest_in_tree);
    }

    // traverse any uncovered sources, which the spec allows us to use our
    // destination number directly for
    for uncovered_range in split_range(
        source_range.clone(),
        translation
            .overlapping(&source_range)
            .map(|v| v.0.clone())
            .collect(),
    ) {
        let current_range = traverse_path(input, rest, *next_path, uncovered_range);
        lowest_bound_seen = lowest_bound_seen.min(current_range);
    }

    lowest_bound_seen
}

/// Splits `main_range` into multiple ranges not covered by `ranges`.
fn split_range(main_range: Range<u64>, mut ranges: Vec<Range<u64>>) -> Vec<Range<u64>> {
    let mut non_intersecting_ranges = Vec::new();
    let mut current_start = main_range.start;

    ranges.sort_by_key(|r| r.start);

    for range in ranges {
        if range.start > current_start {
            non_intersecting_ranges.push(current_start..range.start);
        }

        if range.end > current_start {
            current_start = range.end;
        }
    }

    if current_start < main_range.end {
        non_intersecting_ranges.push(current_start..main_range.end);
    }

    non_intersecting_ranges
}

#[derive(strum::EnumString, Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[strum(serialize_all = "kebab-case")]
enum MapKind {
    Seed,
    Soil,
    Fertilizer,
    Water,
    Light,
    Temperature,
    Humidity,
    Location,
}

#[derive(Debug, Hash, Copy, Clone, PartialEq, Eq)]
struct Translation {
    from: MapKind,
    to: MapKind,
}

impl From<(MapKind, MapKind)> for Translation {
    fn from((from, to): (MapKind, MapKind)) -> Self {
        Self { from, to }
    }
}

#[derive(Debug)]
struct Input {
    seeds: Vec<u64>,
    maps: HashMap<Translation, RangeMap<u64, u64>>,
}

/// parse entire input
fn parse_input(rest: &str) -> IResult<&str, Input> {
    use nom::{
        bytes::complete::tag, character::complete::digit1, combinator::map_res,
        multi::separated_list1, sequence::delimited,
    };

    let (rest, seeds) = delimited(
        tag("seeds: "),
        separated_list1(tag(" "), map_res(digit1, u64::from_str)),
        tag("\n\n"),
    )(rest)?;
    let (rest, maps) = separated_list1(tag("\n"), parse_single_map)(rest)?;

    Ok((
        rest,
        Input {
            seeds,
            maps: maps.into_iter().collect(),
        },
    ))
}

/// parse header along with each map line
fn parse_single_map(rest: &str) -> IResult<&str, (Translation, RangeMap<u64, u64>)> {
    use nom::multi::many1;

    let (rest, header) = parse_header(rest)?;
    let (rest, lines) = many1(parse_map_line)(rest)?;

    Ok((rest, (header, lines.into_iter().collect())))
}

/// parse `803774611 641364296 1132421037` line
fn parse_map_line(rest: &str) -> IResult<&str, (Range<u64>, u64)> {
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::digit1,
        combinator::{eof, map_res},
        sequence::terminated,
    };

    let (rest, destination) = terminated(map_res(digit1, u64::from_str), tag(" "))(rest)?;
    let (rest, source) = terminated(map_res(digit1, u64::from_str), tag(" "))(rest)?;
    let (rest, size) = terminated(map_res(digit1, u64::from_str), alt((tag("\n"), eof)))(rest)?;

    Ok((rest, (source..source + size, destination)))
}

/// parse `seed-to-soil map:` line
fn parse_header(rest: &str) -> IResult<&str, Translation> {
    use nom::{
        bytes::complete::{tag, take_until},
        combinator::{map, map_res},
        sequence::{separated_pair, terminated},
    };

    map(
        terminated(
            separated_pair(
                map_res(take_until("-"), MapKind::from_str),
                tag("-to-"),
                map_res(take_until(" "), MapKind::from_str),
            ),
            tag(" map:\n"),
        ),
        Translation::from,
    )(rest)
}
