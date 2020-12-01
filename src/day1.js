'use strict';

const fs = require('fs');

// Naive solution since the alternative is way too much thinking
function main() {
	const parts = fs.readFileSync('./input/day1.txt').toString().split('\r\n');

	const numbers = [];

	for(const part of parts) {
		const num = Number(part);

		for(const num1 of numbers) {
			for(const num2 of numbers) {
				if(num + num1 + num2 === 2020) {
					return num * num1 * num2;
				}
			}
		}
		numbers.push(num);
	}
}

console.log(main());
