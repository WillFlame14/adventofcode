use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::collections::HashSet;

#[derive(PartialEq, Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West
}

fn turn(dir: &str, curr: &Direction) -> Direction {
    let dirs = [Direction::North, Direction::East, Direction::South, Direction::West];
    let index = dirs.iter().position(|s| s == curr).unwrap();

    match dir {
        "R" => { dirs[(index + 1) % 4] }
        "L" => { dirs[(4 + index - 1) % 4] },
        _ => panic!("Tried to turn unknown direction {}", dir)
    }
}

fn move_dist(loc: (i32, i32), dir: &Direction, dist: i32, visited: &mut HashSet<(i32, i32)>) -> ((i32, i32), Option<(i32, i32)>) {
    let (x, y) = loc;

    let (new_x, new_y) = match *dir {
        Direction::North => { (x, y + dist) },
        Direction::East  => { (x + dist, y) },
        Direction::South => { (x, y - dist) },
        Direction::West  => { (x - dist, y) }
    };

    let mut already_visited = None;

    for i in 1..dist {
        let curr = (x + ((new_x - x) / dist) * i, y + ((new_y - y) / dist) * i);

        if visited.contains(&curr) && already_visited.is_none() {
            already_visited = Some(curr);
        }
        else {
            visited.insert(curr);
        }
    }

    ((new_x, new_y), already_visited)
}

fn manhattan_dist(loc: (i32, i32)) -> i32 {
    return loc.0.abs() + loc.1.abs();
}

fn main() {
    let path = Path::new("input.txt");
    let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", display, why),
        Ok(file) => file,
    };

    let mut s = String::new();
    file.read_to_string(&mut s).unwrap();

    let parts = s.trim().split(", ");

    let mut loc = (0, 0);
    let mut dir = Direction::North;
    let mut visited = HashSet::new();
    let mut already_visited = None;

    for p in parts {
        let (first, rest) = p.split_at(1);
        let distance = rest.parse::<i32>().unwrap();

        dir = turn(first, &dir);
        let result = move_dist(loc, &dir, distance, &mut visited);
        loc = result.0;

        if already_visited.is_none() && result.1.is_some() {
            already_visited = result.1;
        }
    }

    println!("part 1 {}", manhattan_dist(loc));
    println!("part 2 {}", manhattan_dist(already_visited.unwrap()));
}