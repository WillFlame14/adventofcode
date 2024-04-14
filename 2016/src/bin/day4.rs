use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn parse_end(last: &str) -> (i32, String) {
    let mut sector_id = String::new();
    let mut checksum = String::new();

    for c in last.chars() {
        if c.is_numeric() {
            sector_id.push(c);
            continue;
        }

        if c == '[' || c == ']' {
            continue;
        }

        checksum.push(c);
    }

    (sector_id.parse::<i32>().unwrap(), checksum)
}

fn part1(lines: Vec<&str>) -> i32 {
    let mut total = 0;
    
    for line in lines {
        let parts: Vec<&str> = line.split("-").collect();
        let mut letter_freqs: HashMap<char, i32> = HashMap::new();

        for i in 0..parts.len() - 1 {
            for c in parts[i].chars() {
                *letter_freqs.entry(c).or_insert(0) += 1;
            }
        }

        let mut freqs: Vec<_> = letter_freqs.iter().collect();
        freqs.sort_by(|a, b| {
            if b.1 != a.1 {
                b.1.cmp(a.1)
            }
            else {
                a.0.cmp(b.0)
            }});

        let top5: String = freqs.iter().take(5).map(|f| f.0).collect();
        let (sector_id, checksum) = parse_end(parts.last().unwrap());

        if top5 == checksum {
            total += sector_id;
        }
    }
    total
}

fn part2(lines: Vec<&str>) {
    for line in lines {
        let parts: Vec<&str> = line.split("-").collect();
        let (sector_id, _) = parse_end(parts.last().unwrap());
        let mut new_strs = Vec::new();

        for i in 0..parts.len() - 1 {
            let mut new_str = String::new();
            for c in parts[i].chars() {
                new_str.push((((c as i32 - 'a' as i32 + sector_id) % 26) + ('a' as i32)) as u8 as char)
            }
            new_strs.push(new_str);
        }

        if new_strs.iter().any(|s| s.contains("north")) {
            println!("strs {:?} sector_id {}", new_strs, sector_id);
        }
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

    let lines: Vec<&str> = s.trim().split("\n").collect();

    println!("part 1 {}", part1(lines.clone()));
    part2(lines);
}