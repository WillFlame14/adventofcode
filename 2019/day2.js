'use strict';

const fs = require('fs');

async function main(input) {
	const program = input || fs.readFileSync('input.txt').toString().split(',').map(val => Number(val));

	const promise = new Promise((resolve) => {
		for(let i = 0; i < program.length; i++) {
			const opcode = program[i];
			let value;

			switch(opcode) {
				case 1:
					value = program[program[i + 1]] + program[program[i + 2]];
					program[program[i + 3]] = value;
					i += 3;
					break;
				case 2:
					value = program[program[i + 1]] * program[program[i + 2]];
					program[program[i + 3]] = value;
					i += 3;
					break;
				case 99:
					resolve(program[0]);
					break;
			}
		}
	});

	return promise;
}

/**
 * Go through all possibilities, from 0 to 99, of the inputs.
 */
async function main2() {
	const program = fs.readFileSync('input.txt').toString().split(',').map(val => Number(val));

	for(let noun = 0; noun <= 99; noun++) {
		for (let verb = 0; verb <= 99; verb++) {
			program[1] = noun;
			program[2] = verb;

			const result = await main(program.slice());

			if(result === 19690720) {
				return 100 * noun + verb;
			}
		}
	}
	return -1;
}

Promise.all([main(), main2()]).then(results => {
	console.log(results[0]);
	console.log(results[1]);
});
