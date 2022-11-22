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

function run(program, input, i = 0) {
	// eslint-disable-next-line no-constant-condition
	while(true) {
		const instruction = program[i];
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

		let value;

		switch(opcode) {
			case 1: 	// Add
				value = parameters[0] + parameters[1];
				program[program[i + 3]] = value;
				i += 4;
				break;
			case 2: 	// Multiply
				value = parameters[0] * parameters[1];
				program[program[i + 3]] = value;
				i += 4;
				break;
			case 3: 	// Input
				program[program[i + 1]] = input[0];
				// Pop the input
				input.splice(0, 1);
				i += 2;
				break;
			case 4: 	// Output
				i += 2;
				return [program, parameters[0], i];
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
				program[program[i + 3]] = parameters[0] < parameters[1] ? 1 : 0;
				i += 4;
				break;
			case 8: 	// Equals
				program[program[i + 3]] = parameters[0] === parameters[1] ? 1 : 0;
				i += 4;
				break;
			case 99:
				//console.log('PROGRAM TERMINATED');
				return;
			default:
				throw new Error(`encountered unknown opcode ${opcode}`);
		}
	}
}

function run_amplifiers(program, phase_settings) {
	let signal = 0;
	for(const phase of phase_settings) {
		signal = run(program, [phase, signal])[1];
	}
	return signal;
}

function main() {
	const program = fs.readFileSync('input.txt').toString().split(',').map(val => Number(val));
	let max = -1;
	for(let a = 0; a < 5; a++) {
		for(let b = 0; b < 5; b++) {
			if(b === a) {
				continue;
			}
			for(let c = 0; c < 5; c++) {
				if(c === a || c === b) {
					continue;
				}
				for(let d = 0; d < 5; d++) {
					if(d === a || d === b || d === c) {
						continue;
					}
					for(let e = 0; e < 5; e++) {
						if(e === a || e === b || e === c || e === d) {
							continue;
						}
						const signal = run_amplifiers(program, [a, b, c, d, e]);

						if(signal > max) {
							max = signal;
						}
					}
				}
			}
		}
	}
	return max;
}

function run_amplifiers2(base_program, phase_settings) {
	const amplifiers = phase_settings.map((phase) => {
		return { program: base_program.slice(), phase, i: 0 };
	});

	let signal = 0;
	let stopped = 0;

	while(stopped < 5) {
		for(const amplifier of amplifiers) {
			const { program, phase, i } = amplifier;
			const input = i === 0 ? [phase, signal] : [signal];
			const result = run(program, input, i);
			if(result !== undefined) {
				[amplifier.program, signal, amplifier.i] = result;
			}
			else {
				stopped++;
			}
		}
	}

	return signal;
}

function main2() {
	const program = fs.readFileSync('input.txt').toString().split(',').map(val => Number(val));
	let max = -1;
	for(let a = 0; a < 5; a++) {
		for(let b = 0; b < 5; b++) {
			if(b === a) {
				continue;
			}
			for(let c = 0; c < 5; c++) {
				if(c === a || c === b) {
					continue;
				}
				for(let d = 0; d < 5; d++) {
					if(d === a || d === b || d === c) {
						continue;
					}
					for(let e = 0; e < 5; e++) {
						if(e === a || e === b || e === c || e === d) {
							continue;
						}
						const signal = run_amplifiers2(program, [a + 5, b + 5, c + 5, d + 5, e + 5]);

						if(signal > max) {
							max = signal;
						}
					}
				}
			}
		}
	}
	return max;
}

console.log(main());
console.log(main2());
