use std::{
    collections::HashSet,
    io::Read,
    ops::{Add, Rem},
};

fn main() {
    let mut input = String::new();
    std::io::stdin().lock().read_to_string(&mut input).unwrap();
    let (origin, obstructions, canvas_size) = parse_input(input.trim());

    part1(origin, &obstructions, canvas_size);
    part2(origin, &obstructions, canvas_size);
}

fn part1(origin: Point, obstructions: &HashSet<Point>, bounds: Point) {
    eprintln!("{}", bfs(origin, obstructions, 64, bounds));
}

fn part2(origin: Point, obstructions: &HashSet<Point>, canvas_size: Point) {
    let c = canvas_size.0;
    let n = f64::from(26_501_365_i32 / c + 1);
    let r = 26_501_365_i32 % c;

    let x1 = f64::from(bfs(origin, obstructions, r, canvas_size));
    let x2 = f64::from(bfs(origin, obstructions, r + c, canvas_size));
    let x3 = f64::from(bfs(origin, obstructions, r + (c * 2), canvas_size));

    let coeffs = divided_difference([x1, x2, x3]);
    eprintln!("{}", interpolate(&coeffs, n));
}

fn divided_difference<const N: usize>(y: [f64; N]) -> [f64; N] {
    let mut coefficients = [0.0; N];
    let mut divided_diff = y;

    for i in 0..N {
        coefficients[i] = divided_diff[i];

        for j in (i + 1..N).rev() {
            let i = u32::try_from(i).unwrap();
            divided_diff[j] = (divided_diff[j] - divided_diff[j - 1]) / f64::from(i + 1);
        }
    }

    coefficients
}

fn interpolate(coefficients: &[f64], x: f64) -> f64 {
    let mut result = 0.0;
    let mut term = 1.0;

    for (i, &coeff) in coefficients.iter().enumerate() {
        result += coeff * term;
        term *= x - f64::from(u32::try_from(i).unwrap() + 1);
    }

    result
}

fn bfs(origin: Point, obstructions: &HashSet<Point>, max_steps: i32, bounds: Point) -> u32 {
    let mut queue = Vec::new();
    queue.push((origin, 0_i32));

    let mut visited = HashSet::new();
    let mut out = 0;

    while let Some((pos, steps)) = queue.pop() {
        if !visited.insert((pos, steps)) {
            continue;
        }

        if steps == max_steps {
            out += 1;
            continue;
        }

        for d in Direction::ALL {
            let p = pos.go(d);

            if obstructions.contains(&(((p % bounds) + bounds.0) % bounds)) {
                continue;
            }

            queue.push((p, steps + 1));
        }
    }

    out
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct Point(i32, i32);

impl Add<i32> for Point {
    type Output = Self;

    fn add(self, rhs: i32) -> Self::Output {
        Self(self.0 + rhs, self.1 + rhs)
    }
}

impl Rem for Point {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Self(self.0 % rhs.0, self.1 % rhs.1)
    }
}

impl Point {
    #[must_use]
    pub fn go(self, d: Direction) -> Self {
        let Self(x, y) = self;

        let (nx, ny) = match d {
            Direction::Left => (x - 1, y),
            Direction::Right => (x + 1, y),
            Direction::Up => (x, y - 1),
            Direction::Down => (x, y + 1),
        };

        Self(nx, ny)
    }
}

#[derive(Copy, Clone)]
pub enum Direction {
    Left,
    Right,
    Up,
    Down,
}

impl Direction {
    const ALL: [Self; 4] = [Self::Left, Self::Right, Self::Up, Self::Down];
}

fn parse_input(s: &str) -> (Point, HashSet<Point>, Point) {
    let mut origin = None;
    let mut obstructions = HashSet::new();

    let mut x = 0;
    let mut y = 0;

    for c in s.chars() {
        if c == '#' {
            obstructions.insert(Point(x, y));
        } else if c == 'S' {
            origin = Some(Point(x, y));
        }

        if c == '\n' {
            x = 0;
            y += 1;
        } else {
            x += 1;
        }
    }

    (origin.unwrap(), obstructions, Point(x, y + 1))
}
