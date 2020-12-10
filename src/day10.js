'use strict';

const fs = require('fs');

const array = [0];
const ways = [1];

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	for(const line of lines) {
		const parts = Number(line);

		array.push(parts);
	}

	array.sort((a, b) => a - b);
	array.push(array[array.length - 1] + 3);

	// Part 1
	/* const diffs = [];
	for(let i = 1; i < array.length; i++) {
		const diff = array[i] - array[i-1];
		if(diffs[diff] === undefined) {
			diffs[diff] = 0;
		}
		diffs[diff]++;
	}

	return diffs[1] * diffs[3]; */

	for(let i = 0; i < array.length - 1; i++) {
		travel(i);
	}
	return ways[ways.length - 1];
}

function travel(index) {
	const curr = array[index];

	for(let i = 0; i < 3; i++) {
		const new_index = index + i + 1;
		if(new_index >= array.length) {
			return;
		}
		if(array[new_index] - curr <= 3) {
			if(ways[array[new_index]] === undefined) {
				ways[array[new_index]] = 0;
			}
			ways[array[new_index]] += ways[curr];
		}
	}
}

console.log(main());
