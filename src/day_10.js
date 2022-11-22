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

	// For some reason JS can only sort arrays lexicographically
	array.sort((a, b) => a - b);

	// Add your device
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

	// Part 2
	// Go through all adapters and dp the next pluggable adapters
	for(let i = 0; i < array.length - 1; i++) {
		// Check whether any of the next 3 largest adapters are pluggable
		for(let j = i + 1; j < array.length; j++) {
			if(array[j] - array[i] <= 3) {
				if(ways[j] === undefined) {
					ways[j] = 0;
				}
				ways[j] += ways[i];
			}
		}
	}
	return ways[ways.length - 1];
}

console.log(main());
