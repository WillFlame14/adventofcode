use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn move_dir(loc: &(i32, i32), dir: &char) -> (i32, i32) {
    let (x, y) = *loc;

    let (mut new_x, mut new_y) = match dir {
        'L' => (x - 1, y),
        'R' => (x + 1, y),
        'U' => (x, y - 1),
        'D' => (x, y + 1),
        x => panic!("unrecognized dir {}", x)
    };

    if new_x < 0 {
        new_x = 0;
    }
    else if new_x > 2 {
        new_x = 2;
    }

    if new_y < 0 {
        new_y = 0;
    }
    else if new_y > 2 {
        new_y = 2;
    }

    (new_x, new_y)
}

fn keypad(loc: &(i32, i32)) -> i32 {
    let (x, y) = loc;

    y * 3 + x + 1
}

fn move_dir2(loc: &(i32, i32), dir: &char) -> (i32, i32) {
    let (x, y) = *loc;

    let (new_x, new_y) = match dir {
        'L' => (x - 1, y),
        'R' => (x + 1, y),
        'U' => (x, y + 1),
        'D' => (x, y - 1),
        x => panic!("unrecognized dir {}", x)
    };

    if ((new_x == 0 || new_x == 4) && new_y != 0) ||
        ((new_x == 1 || new_x == 3) && new_y.abs() > 1) || 
        (new_x == 2 && new_y.abs() > 2) ||
        new_x < 0 || new_x > 4 {
        (x, y)
    }
    else {
        (new_x, new_y)
    }
}

fn keypad2(loc: &(i32, i32)) -> char {
    let (x, y) = *loc;

    match x {
        0 => '5',
        1 => ['A', '6', '2'][(y + 1) as usize],
        2 => ['D', 'B', '7', '3', '1'][(y + 2) as usize],
        3 => ['C', '8', '4'][(y + 1) as usize],
        4 => '9',
        x => panic!("unrecognized keypad ({}, {})", x, y)
    }
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

    let parts = s.trim().split("\n");
    let mut code = String::new();

    let mut loc = (1, 1);

    for p in parts.clone() {
        for c in p.chars() {
            loc = move_dir(&loc, &c);
        }
        code = format!("{}{}", code, keypad(&loc));
    }

    println!("part 1 {}", code);

    code = String::new();
    loc = (0, 0);

    for p in parts {
        for c in p.chars() {
            loc = move_dir2(&loc, &c);
        }
        code = format!("{}{}", code, keypad2(&loc));
    }

    println!("part 2 {}", code);
}