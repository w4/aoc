use std::io::Read;

use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, digit1},
    combinator::map_parser,
    multi::separated_list1,
    sequence::{preceded, terminated},
    IResult,
};

fn main() -> Result<(), anyhow::Error> {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input)?;

    let (rest, input) = parse_input(&input).unwrap();
    assert!(rest.is_empty());

    part1(&input);
    part2(&input);

    Ok(())
}

fn solve(input: &Input) -> i64 {
    let b = (input.prize.y * input.buttons[0].x - input.prize.x * input.buttons[0].y)
        / (input.buttons[1].y * input.buttons[0].x - input.buttons[1].x * input.buttons[0].y);
    let a = (input.prize.x - b * input.buttons[1].x) / input.buttons[0].x;

    if input.buttons[0].x * a + input.buttons[1].x * b == input.prize.x
        && input.buttons[0].y * a + input.buttons[1].y * b == input.prize.y
    {
        3 * a + b
    } else {
        0
    }
}

fn part1(input: &[Input]) {
    eprintln!("{}", input.iter().map(solve).sum::<i64>());
}

fn part2(input: &[Input]) {
    eprintln!(
        "{}",
        input
            .iter()
            .cloned()
            .map(|v| Input {
                prize: Coords {
                    x: v.prize.x + 10_000_000_000_000,
                    y: v.prize.y + 10_000_000_000_000,
                },
                ..v
            })
            .map(|v| solve(&v))
            .sum::<i64>()
    );
}

#[derive(Debug, Copy, Clone)]
pub struct Coords {
    x: i64,
    y: i64,
}

#[derive(Debug, Clone)]
pub struct Input {
    buttons: Vec<Coords>,
    prize: Coords,
}

fn parse_i64(s: &str) -> IResult<&str, i64> {
    map_parser(digit1, nom::character::complete::i64)(s)
}

fn parse_button(s: &str) -> IResult<&str, Coords> {
    let (s, _b) = preceded(tag("Button "), alpha1)(s)?;
    let (s, x) = preceded(tag(": X+"), parse_i64)(s)?;
    let (s, y) = preceded(tag(", Y+"), parse_i64)(s)?;
    Ok((s, Coords { x, y }))
}

fn parse_prize(s: &str) -> IResult<&str, Coords> {
    let (s, x) = preceded(tag("Prize: X="), parse_i64)(s)?;
    let (s, y) = preceded(tag(", Y="), parse_i64)(s)?;
    Ok((s, Coords { x, y }))
}

fn parse_block(s: &str) -> IResult<&str, Input> {
    let (s, buttons) = terminated(separated_list1(tag("\n"), parse_button), tag("\n"))(s)?;
    let (s, prize) = terminated(parse_prize, tag("\n"))(s)?;
    Ok((s, Input { buttons, prize }))
}

fn parse_input(s: &'_ str) -> IResult<&'_ str, Vec<Input>> {
    separated_list1(tag("\n"), parse_block)(s)
}
