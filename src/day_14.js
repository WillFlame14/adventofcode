'use strict';

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let mask = '';
	const memory = [];

	for(const line of lines) {
		const parts = line.split(' ');

		if(parts[0] === 'mask') {
			mask = parts[2];
		}
		else {
			const index = Number(parts[0].substring(4, parts[0].length - 1));
			const num = Number(parts[2]).toString(2);
			let value = '';

			for(let i = 0; i < mask.length; i++) {
				const char = mask.charAt(mask.length - i - 1);

				if(char === '1') {
					value = '1' + value;
				}
				else if(char === '0') {
					value = '0' + value;
				}
				else {
					if(i >= num.length) {
						value = '0' + value;
					}
					else {
						value = num.charAt(num.length - i - 1) + value;
					}
				}
			}

			memory[index] = parseInt(value, 2);
		}
	}

	return memory.reduce((total, num) => total += Number(num) || 0, 0);
}

const memory = [];
let sum = 0;

function main2() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let mask = '';

	for(const line of lines) {
		const parts = line.split(' ');

		if(parts[0] === 'mask') {
			mask = parts[2];
		}
		else {
			const index = Number(parts[0].substring(4, parts[0].length - 1)).toString(2);
			const num = Number(parts[2]);

			let value = '';

			for(let i = 0; i < mask.length; i++) {
				const char = mask.charAt(mask.length - i - 1);

				if(char === '1') {
					value = '1' + value;
				}
				else if(char === '0') {
					value = (index.charAt(index.length - i - 1) || '0') + value;
				}
				else {
					value = 'X' + value;
				}
			}
			assign(value, num);
		}
	}

	return sum;
}

function assign(address, value) {
	const index = address.indexOf('X');
	if(index === -1) {
		const loc = parseInt(address, 2);
		sum += value - (memory[loc] || 0);
		memory[loc] = value;
	}
	else {
		assign(address.substring(0, index) + '0' + address.substring(index + 1), value);
		assign(address.substring(0, index) + '1' + address.substring(index + 1), value);
	}
}

console.log(main());
console.log(main2());
