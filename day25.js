'use strict';

const fs = require('fs');

const readline = require('readline').createInterface({
	input: process.stdin,
	output: process.stdout
});

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

function run(program, input = [], i = 0, relative_base = 0) {
	const output = [];
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
				if(input.length <= 0) {
					return { program, output, i, relative_base };
				}

				// Read and pop the input
				program[immediates[0]] = input[0];
				input.splice(0, 1);
				i += 2;
				break;
			case 4: 	// Output
				i += 2;
				output.push(parameters[0]);
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
				return { output, terminate: true };
			default:
				throw new Error(`encountered unknown opcode ${opcode}`);
		}
	}
}

function displayOutput(output) {
	let line = '';
	for(const char of output) {
		if(char !== 10) {
			line += String.fromCharCode(char);
		}
		else {
			console.log(line);
			line = '';
		}
	}
}

function runWithInput(o_program, o_input, o_i, o_relative_base) {
	const result = run(o_program, o_input, o_i, o_relative_base);

	if(result === undefined) {
		console.log('got undefined output???');
		readline.close();
		return;
	}

	const { program, output, i, relative_base, terminate } = result;
	displayOutput(output);

	if(terminate) {
		console.log('== PROGRAM TERMINATED ==');
		readline.close();
		return;
	}

	const input = [];
	readline.question('', (input_text) => {
		for(let j = 0; j < input_text.length; j++) {
			input.push(input_text.charCodeAt(j));
		}
		input.push(10);

		runWithInput(program, input, i, relative_base);
	});
}

function main() {
	const program = fs.readFileSync('input.txt').toString().split(',').map(val => Number(val));

	const input = [];
	runWithInput(program, input);
}

main();
