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
				console.log(parameters[0]);
				break;
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
	run(program, [1]);
}

function main2() {
	const program = fs.readFileSync('input.txt').toString().split(',').map(val => Number(val));
	run(program, [2]);
}

console.log(main());
console.log(main2());
