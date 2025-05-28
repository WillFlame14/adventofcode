use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

#[derive(Debug, Clone, Copy)]
pub struct Register(i32);

enum LVal {
	Reg(String),
	Val(i32),
}

fn parse_lval(str: &str) -> LVal {
	match str.chars().next().unwrap() {
		'a'..='d' => LVal::Reg(str.to_string()),
		_ => LVal::Val(str.parse().unwrap())
	}
}

enum Instruction {
	Cpy(LVal, String),
	Inc(String),
	Dec(String),
	Jnz(LVal, i32),
}

fn parse_instruction(str: &str) -> Instruction {
	let parts: Vec<&str> = str.split_whitespace().collect();
	match parts[0] {
		"cpy" => Instruction::Cpy(parse_lval(parts[1]), parts[2].to_string()),
		"inc" => Instruction::Inc(parts[1].to_string()),
		"dec" => Instruction::Dec(parts[1].to_string()),
		"jnz" => Instruction::Jnz(parse_lval(parts[1]), parts[2].parse().unwrap()),
		_ => panic!("Invalid instruction")
	}
}

pub struct Program {
	mem: [Register; 4],
	pc: i32,
}

fn letter_to_index(letter: &str) -> usize {
	let letter = letter.chars().next().unwrap();
	letter as usize - 'a' as usize
}

impl Program {
	fn new() -> Program {
		Program {
			mem: [Register(0); 4],
			pc: 0,
		}
	}

	fn set(&mut self, reg: &str, value: i32) {
		self.mem[letter_to_index(reg)].0 = value;
	}

	fn get(&self, reg: &str) -> i32 {
		self.mem[letter_to_index(reg)].0
	}

	fn execute(&mut self, inst: &Instruction) {
		match inst {
			Instruction::Cpy(l, dest) => self.set(dest, match l {
				LVal::Reg(reg) => self.get(reg),
				LVal::Val(v) => *v
			}),
			Instruction::Inc(reg) => self.set(reg, self.get(reg) + 1),
			Instruction::Dec(reg) => self.set(reg, self.get(reg) - 1),
			Instruction::Jnz(l, offset) => if match l {
				LVal::Reg(reg) => self.get(reg),
				LVal::Val(v) => *v,
			} != 0 {
				self.pc += offset - 1;
			},
		}
	}

	fn run(&mut self, instructions: &[Instruction]) {
		while self.pc < instructions.len() as i32 {
			self.execute(&instructions[self.pc as usize]);
			self.pc += 1;
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

    let mut program = Program::new();
    // Uncomment for part 2
    // program.set("c", 1);

    let insts: Vec<Instruction> = s.trim().split("\n").map(|line| parse_instruction(line)).collect();

    program.run(&insts);
    println!("{}", program.get("a"));
}
