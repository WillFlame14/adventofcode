'use strict';

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let num, max = 0, min = 9999999;
	const ids = [];

	for(const line of lines) {
		// Read row and col as binary
		const row = parseInt(line.substring(0, 7).replace(/F/g, '0').replace(/B/g, '1'), 2);
		const col = parseInt(line.substring(7, 10).replace(/R/g, '1').replace(/L/g, '0'), 2);

		const id = 8 * row + col;

		if(id > max) {
			max = id;
		}
		if(id < min) {
			min = id;
		}

		ids.push(id);
	}

	for(let i = min; i < max; i++) {
		if(!ids.includes(i)) {
			num = i;
		}
	}

	return num;
}

console.log(main());
