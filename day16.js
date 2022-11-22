'use strict';

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	const turns = 100;
	let signal = lines[0];
	const pattern = [0, 1, 0, -1];

	for(let i = 0; i < turns; i++) {
		let new_str = '';
		for(let j = 1; j <= signal.length; j++) {
			let value = 0, index = 1;
			for(let k = 0; k < signal.length; k++) {
				value += Number(signal[k]) * pattern[Math.floor((index % (4 * j)) / j)];
				index++;
			}
			new_str += `${Math.abs(value) % 10}`;
		}
		signal = new_str;
	}
	return signal.substring(0, 8);
}

/**
 * Basically, cheese the entire problem by noticing that the offset takes up more than half of the full length of the input.
 * Thus, we only care about the numbers beyond the offset, since the ones before are all multiplied by zero.
 * In fact, all the remaining numbers are simply summed together.
 */
function main2() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	const turns = 100;

	let signal = lines[0];
	const offset = Number(lines[0].substring(0, 7));

	for(let i = 1; i < 10000; i++) {
		signal += lines[0];
	}

	signal = signal.substring(offset);

	let sum = 0;

	for(let i = 0; i < signal.length; i++) {
		sum += Number(signal[i]);
	}

	for(let i = 0; i < turns; i++) {
		let new_str = '';
		let new_sum = 0;
		for(let j = 0; j < signal.length; j++) {
			const num = sum % 10;
			new_str += `${num}`;
			sum -= Number(signal[j]);
			new_sum += num;
		}
		signal = new_str;
		sum = new_sum;
	}

	return signal.substring(0, 8);
}

console.log(main());
console.log(main2());
