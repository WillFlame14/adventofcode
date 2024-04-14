use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn parse_ints(line: String) -> Vec<i32> {
    line.trim().split_whitespace().map(|p| p.parse::<i32>().unwrap()).collect::<Vec<_>>()
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

    let lines = s.trim().split("\n");
    let mut valid_triangles = 0;

    for line in lines.clone() {
        let mut parts = parse_ints(line.to_string());
        parts.sort();

        if parts[0] + parts[1] > parts[2] {
            valid_triangles += 1;
        }
    }

    println!("part 1 {}", valid_triangles);

    valid_triangles = 0;

    for line_group in lines.collect::<Vec<_>>().chunks(3) {
        let mut triangles = vec!(Vec::new(), Vec::new(), Vec::new());

        for line in line_group {
            let parts = parse_ints(line.to_string());

            for i in 0..3 {
                triangles[i].push(parts[i]);
            }
        }

        for i in 0..3 {
            triangles[i].sort();

            if triangles[i][0] + triangles[i][1] > triangles[i][2] {
                valid_triangles += 1;
            }
        }
    }

    println!("part 2 {}", valid_triangles);
}