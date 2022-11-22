'use strict';

const fs = require('fs');

// Number of parameters (not including write positions)
const paramLengths = {
	1: 2,
	2: 2,
	3: 0,
	4: 1,
	5: 2,
	6: 2,
	7: 2,
	8: 2
};

function main(program, input) {
	if(program === undefined) {
		program = fs.readFileSync('input.txt').toString().split(',').map(val => Number(val));
	}

	let i = 0;

	// eslint-disable-next-line no-constant-condition
	while(true) {
		//console.log(program);
		const instruction = program[i];
		//console.log(instruction);
		const opcode = instruction % 100;
		const padded_instruction = `${instruction}`.substring(0, `${instruction}`.length - 2).padStart(paramLengths[opcode], 0);

		const modes = padded_instruction.split("").reverse();
		const parameters = [];
		for (const [index, mode] of modes.entries()) {
			const immediate = program[i + index + 1];
			// Position mode
			if(mode === "0") {
				parameters.push(program[immediate]);
			}
			// Immediate mode
			else if(mode === "1") {
				parameters.push(immediate);
			}
		}
		//parameters.reverse();

		//console.log(`modes ${modes}`);
		//console.log(`parameters ${parameters}`);

		let value;

		switch(opcode) {
			case 1: 	// Add
				value = parameters[0] + parameters[1];
				program[program[i + 3]] = value;
				//console.log(`setting program[${program[i + 3]} to ${value}`);
				i += 4;
				break;
			case 2: 	// Multiply
				value = parameters[0] * parameters[1];
				program[program[i + 3]] = value;
				//console.log(`setting program[${program[i + 3]} to ${value}`);
				i += 4;
				break;
			case 3: 	// Input
				program[program[i + 1]] = input;
				//console.log(`setting program[${program[i + 1]} to ${input}`);
				i += 2;
				break;
			case 4: 	// Output
				console.log('OUTPUT ' + parameters[0]);
				i += 2;
				break;
			case 5: 	// Jump-if-true
				if(parameters[0] !== 0) {
					i = parameters[1];
					//console.log(`jumping to ${parameters[1]}`);
				}
				else {
					i += 3;
					//console.log('failed jump, now at ' + i);
				}
				break;
			case 6: 	// Jump-if-false
				if(parameters[0] === 0) {
					i = parameters[1];
					//console.log(`jumping to ${parameters[1]}`);
				}
				else {
					i += 3;
					//console.log('failed jump, now at ' + i);
				}
				break;
			case 7: 	// Less than
				program[program[i + 3]] = parameters[0] < parameters[1] ? 1 : 0;
				i += 4;
				break;
			case 8: 	// Equals
				program[program[i + 3]] = parameters[0] === parameters[1] ? 1 : 0;
				//console.log(`comparing ${parameters[0]} with ${parameters{1}}`)
				i += 4;
				break;
			case 99:
				console.log('PROGRAM TERMINATED');
				return;
			default:
				throw new Error(`encountered unknown opcode ${opcode}`);
		}
	}
}

// Part 1
main(undefined, 1);
// Part 2
main(undefined, 5);
