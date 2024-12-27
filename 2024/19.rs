use std::{collections::HashMap, io::Read};

use anyhow::Result;
use nom::IResult;
use trie_hard::TrieHard;

fn main() -> Result<()> {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input)?;

    let (_, input) = parse_input(&input).unwrap();

    let mut possible = 0;
    let mut rearrangements = 0;

    for wanted in &input.wanted {
        let res = get_towels_for_patterns(wanted, &input.towels, &mut HashMap::new());
        possible += res.min(1);
        rearrangements += res;
    }

    eprintln!("{possible}");
    eprintln!("{rearrangements}");

    Ok(())
}

fn get_towels_for_patterns<'b>(
    wanted: &'b str,
    towels: &TrieHard<'_, &'_ str>,
    cache: &mut HashMap<&'b str, u64>,
) -> u64 {
    if wanted.is_empty() {
        return 1;
    }

    let mut possibilities = 0;

    for i in (0..wanted.len()).rev() {
        let (wanted, rest) = wanted.split_at(i + 1);

        if towels.get(wanted).is_some() {
            if let Some(cached) = cache.get(rest) {
                possibilities += cached;
                continue;
            }

            let value = get_towels_for_patterns(rest, towels, cache);
            cache.insert(rest, value);
            possibilities += value;
        }
    }

    possibilities
}

#[derive(Debug)]
pub struct Input<'a> {
    towels: TrieHard<'a, &'a str>,
    wanted: Vec<&'a str>,
}

fn parse_input(input: &str) -> IResult<&str, Input> {
    use nom::{
        bytes::complete::tag, character::complete::alpha1, combinator::map, multi::separated_list1,
        sequence::separated_pair,
    };

    let parse_towels = map(separated_list1(tag(", "), alpha1), TrieHard::from_iter);
    let parse_wanted = separated_list1(tag("\n"), alpha1);

    map(
        separated_pair(parse_towels, tag("\n\n"), parse_wanted),
        |(towels, wanted)| Input { towels, wanted },
    )(input)
}
