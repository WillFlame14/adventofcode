'use strict';

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	const map = [];

	for(const line of lines) {
		const row = [];
		for(let i = 0; i < line.length; i++) {
			row.push(line.charAt(i));
		}
		map.push(row);
	}

	// Part 1
	// return travel(map, 3, 1);

	// Part 2
	return travel(map, 1, 1) * travel(map, 3, 1) * travel(map, 5, 1) * travel(map, 7, 1) * travel(map, 1, 2);
}

function travel(map, right, down) {
	let num = 0, height = 0, length = 0;
	const full_len = map[0].length;

	while(height < map.length) {
		if(map[height][length] === '#') {
			num++;
		}

		length += right;
		height += down;

		if(length >= full_len) {
			length = length % full_len;
		}
	}
	return num;
}

console.log(main());
