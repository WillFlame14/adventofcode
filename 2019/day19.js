'use strict';

const fs = require('fs');

// Number of parameters (not including write positions)
const paramLengths = {
	1: 3,
	2: 3,
	3: 1,
	4: 1,
	5: 2,
	6: 2,
	7: 3,
	8: 3,
	9: 1
};

function run(program, input, i = 0, relative_base = 0) {
	// eslint-disable-next-line no-constant-condition
	while(true) {
		const instruction = program[i];
		const opcode = instruction % 100;
		const padded_instruction = `${instruction}`.substring(0, `${instruction}`.length - 2).padStart(paramLengths[opcode], 0);

		const modes = padded_instruction.split("").reverse();
		const immediates = [];
		const parameters = [];
		for (const [index, mode] of modes.entries()) {
			const immediate = program[i + index + 1];
			// Position mode
			if(mode === "0") {
				parameters.push(program[immediate] || 0);
				immediates.push(immediate);
			}
			// Immediate mode
			else if(mode === "1") {
				parameters.push(immediate);
				immediates.push(immediate);
			}
			// Relative mode
			else if(mode === "2") {
				parameters.push(program[immediate + relative_base] || 0);
				immediates.push(immediate + relative_base);
			}
		}

		let value;

		switch(opcode) {
			case 1: 	// Add
				value = parameters[0] + parameters[1];
				program[immediates[2]] = value;
				i += 4;
				break;
			case 2: 	// Multiply
				value = parameters[0] * parameters[1];
				program[immediates[2]] = value;
				i += 4;
				break;
			case 3: 	// Input
				program[immediates[0]] = input[0];
				// Pop the input
				input.splice(0, 1);
				i += 2;
				break;
			case 4: 	// Output
				i += 2;
				//return [program, parameters[0], i];
				return parameters[0];
				// break;
			case 5: 	// Jump-if-true
				if(parameters[0] !== 0) {
					i = parameters[1];
				}
				else {
					i += 3;
				}
				break;
			case 6: 	// Jump-if-false
				if(parameters[0] === 0) {
					i = parameters[1];
				}
				else {
					i += 3;
				}
				break;
			case 7: 	// Less than
				program[immediates[2]] = parameters[0] < parameters[1] ? 1 : 0;
				i += 4;
				break;
			case 8: 	// Equals
				program[immediates[2]] = parameters[0] === parameters[1] ? 1 : 0;
				i += 4;
				break;
			case 9:
				relative_base += parameters[0];
				i += 2;
				break;
			case 99:
				//console.log('PROGRAM TERMINATED');
				return;
			default:
				throw new Error(`encountered unknown opcode ${opcode}`);
		}
	}
}

function main() {
	const program = fs.readFileSync('input.txt').toString().split(',').map(val => Number(val));
	
	let sum = 0;
	const grid = [];

	for(let x = 0; x < 50; x++) {
		let row = "";
		for(let y = 0; y < 50; y++) {
			const result = run(program.slice(), [x, y]);
			row += result === 1 ? '#' : '.';
			// console.log(x + ' ' + y + ' ' + result);
			sum += run(program.slice(), [x, y]);
		}
		grid.push(row);
	}

	for(const line of grid) {
		console.log(line);
	}
	return sum;
}

function confirm(program, try_x, try_y) {
	return run(program.slice(), [try_x + 99, try_y]) === 1 &&
		run(program.slice(), [try_x, try_y + 99]) === 1 &&
		run(program.slice(), [try_x + 99, try_y + 99]) === 1;
}

function main2() {
	const program = fs.readFileSync('input.txt').toString().split(',').map(val => Number(val));

	let x = 1000, y = 0;
	let row_found = false, last_y = -1;
	while(true) {
		const result = run(program.slice(), [x, y]);
		console.log(x + ' ' + y + ' ' + result);
		if(result === 1) {
			row_found = true;
			if(last_y === -1) {
				last_y = y;
			}
			if(confirm(program.slice(), x, y)) {
				return [x, y];
			}
			else {
				// Keep trying
				y++;
			}
		}
		else {
			if(row_found) {
				x++;
				y = last_y;
				last_y = -1;
				row_found = false;
			}
			else {
				y++;
			}
		}
	}
}

//console.log(main());
console.log(main2());
