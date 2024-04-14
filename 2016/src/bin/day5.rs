fn part1(input: &str) -> String {
    let mut password = String::new();
    let mut i = 0;

    while password.len() < 8 {
        let hash = format!("{:?}", md5::compute(format!("{}{}", input, i)));

        if hash.starts_with("00000") {
            password.push(hash.chars().nth(5).unwrap());
        }
        i += 1;
    }
    password
}

fn part2(input: &str) -> String {
    let mut password = Vec::new();
    for _ in 0..8 {
        password.push('.');
    }

    let mut i = 0;
    let mut pw_filled = 0;

    while pw_filled < 8 {
        let hash = format!("{:?}", md5::compute(format!("{}{}", input, i)));

        if hash.starts_with("00000") {
            let position = hash.chars().nth(5).unwrap();

            if position.is_numeric() {
                let index = position.to_digit(10).unwrap() as usize;

                if index < 8 && password[index] == '.' {
                    password[index] = hash.chars().nth(6).unwrap();
                    pw_filled += 1;
                }
            }
        }
        i += 1;
    }
    password.into_iter().collect()
}

fn main() {
    let input = "wtnhxymk";

    println!("part 1 {}", part1(&input));
    println!("part 2 {}", part2(&input));
}