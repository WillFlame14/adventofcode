'use strict';

const fs = require('fs');

const instructions = [];

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	for(const line of lines) {
		const parts = line.split(' ');
		const op = parts[0];
		const change = Number(parts[1]);

		instructions.push({ op, change });
	}

	let attempt = 1;		// The instruction to toggle
	let num = travel();

	while(num === null) {
		attempt++;
		num = travel(attempt);
	}

	return num;
}

function travel(changeIndex = -1) {
	const visited = new Set();
	let index = 0, num = 0;

	while(!visited.has(index) && index !== instructions.length) {
		visited.add(index);
		const {op, change} = instructions[index];

		switch(op) {
			case 'acc':
				num += change;
				index++;
				break;
			case 'jmp':
				if(index === changeIndex) {
					index++;
				}
				else {
					index += change;
				}
				break;
			case 'nop':
				if(index === changeIndex) {
					index += change;
				}
				else {
					index++;
				}
				break;
		}
	}

	if(index === instructions.length) {
		return num;
	}
	else {
		return null;
	}
}

console.log(main());
