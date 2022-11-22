'use strict';

const fs = require('fs');

function part1() {
	const lines = fs.readFileSync('./input.txt').toString().split('\r\n');

	let num = 0;

	for(const line of lines) {
		const parts = line.split(' ');

		const digits = parts[0].split('-');
		const min = Number(digits[0]);
		const max = Number(digits[1]);

		const letter = parts[1].charAt(0);
		let counter = 0;

		for(let i = 0; i < parts[2].length; i++) {
			if(parts[2].charAt(i) === letter) {
				counter++;
			}
		}

		if(counter >= min && counter <= max) {
			num++;
		}
	}
	return num;
}

function part2() {
	const lines = fs.readFileSync('./input.txt').toString().split('\r\n');

	let num = 0;

	for(const line of lines) {
		const parts = line.split(' ');

		const digits = parts[0].split('-');

		const first = Number(digits[0]);
		const second = Number(digits[1]);

		const letter = parts[1].charAt(0);
		let counter = 0;

		if(parts[2].charAt(first - 1) === letter) {
			counter++;
		}

		if(parts[2].charAt(second - 1) === letter) {
			counter++;
		}

		if(counter === 1) {
			num++;
		}
	}
	return num;
}

console.log(part1());
console.log(part2());
