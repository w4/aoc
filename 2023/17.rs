use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap},
    io::Read,
};

fn main() {
    let mut input = Vec::new();
    std::io::stdin().lock().read_to_end(&mut input).unwrap();
    let input = parse_input(&input);

    part1(&input);
    println!();
    part2(&input);
}

fn part1(input: &[Vec<u8>]) {
    let start = (0, 0);
    let end = (input[0].len() - 1, input.len() - 1);

    let (distance, path) = dijkstra(input, start, end, 0, 3).unwrap();

    print_grid(input, &path);
    eprintln!("part 1: {distance:?}");
}

fn part2(input: &[Vec<u8>]) {
    let start = (0, 0);
    let end = (input[0].len() - 1, input.len() - 1);

    let (distance, path) = dijkstra(input, start, end, 4, 10).unwrap();

    print_grid(input, &path);
    eprintln!("part 1: {distance:?}");
}

#[allow(clippy::type_complexity)]
fn dijkstra(
    input: &[Vec<u8>],
    start: (usize, usize),
    end: (usize, usize),
    min_distance: u32,
    max_distance: u32,
) -> Option<(u32, HashMap<(usize, usize), Direction>)> {
    let mut min_cost: HashMap<((usize, usize), Direction, u32), u32> = HashMap::new();
    let mut queue = BinaryHeap::new();

    queue.push(Reverse(NodeInfo {
        node: start,
        cost: 0,
        step_count: 0,
        last_direction: Direction::None,
        path: HashMap::new(),
    }));

    while let Some(Reverse(NodeInfo {
        node: node @ (x, y),
        cost,
        step_count,
        last_direction,
        path,
    })) = queue.pop()
    {
        if node == end && step_count >= min_distance {
            return Some((cost, path));
        }

        if cost
            > *min_cost
                .get(&(node, last_direction, step_count))
                .unwrap_or(&u32::MAX)
        {
            continue;
        }

        for (neighbour_pos @ (neighbour_x, neighbour_y), direction) in [
            ((x + 1, y), Direction::Right),
            ((x.wrapping_sub(1), y), Direction::Left),
            ((x, y.wrapping_sub(1)), Direction::Up),
            ((x, y + 1), Direction::Down),
        ] {
            if direction.invert() == last_direction {
                continue;
            }

            if step_count == max_distance && direction == last_direction {
                continue;
            }

            if step_count < min_distance
                && direction != last_direction
                && last_direction != Direction::None
            {
                continue;
            }

            let Some(next_cost) = input.get(neighbour_y).and_then(|x| x.get(neighbour_x)) else {
                continue;
            };

            let step_count = if direction == last_direction {
                step_count + 1
            } else {
                1
            };

            let next = NodeInfo {
                node: neighbour_pos,
                cost: cost + u32::from(*next_cost),
                step_count,
                last_direction: direction,
                path: {
                    let mut p = path.clone();
                    p.insert(neighbour_pos, direction);
                    p
                },
            };

            if next.cost
                < *min_cost
                    .get(&(neighbour_pos, direction, step_count))
                    .unwrap_or(&u32::MAX)
            {
                min_cost.insert((neighbour_pos, direction, step_count), next.cost);
                queue.push(Reverse(next));
            }
        }
    }

    None
}

fn print_grid(input: &[Vec<u8>], visited: &HashMap<(usize, usize), Direction>) {
    for (y, a) in input.iter().enumerate() {
        for (x, v) in a.iter().enumerate() {
            match visited.get(&(x, y)) {
                Some(Direction::Up) => print!("^"),
                Some(Direction::Down) => print!("v"),
                Some(Direction::Left) => print!("<"),
                Some(Direction::Right) => print!(">"),
                Some(Direction::None) => print!("*"),
                None => print!("{v}"),
            }
        }

        println!();
    }
}

#[derive(Eq, PartialEq, Debug)]
struct NodeInfo {
    node: (usize, usize),
    cost: u32,
    step_count: u32,
    last_direction: Direction,
    path: HashMap<(usize, usize), Direction>,
}

impl Ord for NodeInfo {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.cost.cmp(&other.cost)
    }
}

impl PartialOrd for NodeInfo {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
    None,
}

impl Direction {
    fn invert(self) -> Self {
        match self {
            Self::Up => Self::Down,
            Self::Down => Self::Up,
            Self::Left => Self::Right,
            Self::Right => Self::Left,
            Self::None => Self::None,
        }
    }
}

fn parse_input(input: &[u8]) -> Vec<Vec<u8>> {
    std::str::from_utf8(input)
        .unwrap()
        .split('\n')
        .map(|v| {
            v.chars()
                .map(|c| u8::try_from(c.to_digit(10).unwrap()).unwrap())
                .collect()
        })
        .collect()
}
