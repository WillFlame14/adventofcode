use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn parts(lines: &Vec<&str>) -> (String, String) {
    let len = lines[0].len();
    let mut hashmaps: Vec<HashMap<char, i32>> = vec![HashMap::new(); len];

    for line in lines {
        let mut chars = line.chars();
        for i in 0..len {
            let c = chars.next().unwrap();
            *hashmaps[i].entry(c).or_insert(0) += 1;
        }
    }
    
    let part1 = hashmaps.iter().map(|hashmap| {
        let mut freqs: Vec<_> = hashmap.iter().collect();
        freqs.sort_by(|a, b| b.1.cmp(a.1));

        freqs[0].0
    }).collect();

    let part2 = hashmaps.iter().map(|hashmap| {
        let mut freqs: Vec<_> = hashmap.iter().collect();
        freqs.sort_by(|a, b| a.1.cmp(b.1));

        freqs[0].0
    }).collect();

    (part1, part2)
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

    let lines: Vec<&str> = s.trim().split("\n").collect();

    let (part1, part2) = parts(&lines);
    println!("part 1 {}", part1);
    println!("part 2 {}", part2);
}