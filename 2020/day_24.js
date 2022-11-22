'use strict';

// We've gone back to demonstrating my power over if statements 8)

const fs = require('fs');

const turns = 100;

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	const valid = new Set();
	const valid_raw = ['e', 'se', 'ne', 'w', 'sw', 'nw'];
	for(const dir of valid_raw) {
		valid.add(dir);
	}

	const tiles = new Map();

	for(const line of lines) {
		let x = 50, y = 50;

		let index = 0, instruction = '';

		while(index !== line.length) {
			// Attack onto 'instruction' until valid
			while(!valid.has(instruction)) {
				instruction += line.charAt(index);
				index++;
			}

			// Imagine a hexagonal grid overlayed on top of a Cartesian grid.

			switch(instruction) {
				case 'e':
					x += 1;
					break;
				case 'se':
					x += 0.5;
					y -= 1;
					break;
				case 'ne':
					x += 0.5;
					y += 1;
					break;
				case 'w':
					x -= 1;
					break;
				case 'sw':
					x -= 0.5;
					y -= 1;
					break;
				case 'nw':
					x -= 0.5;
					y += 1;
					break;
			}
			instruction = '';
		}

		const location = x + ' ' + y;

		// Black is false, white is true. All tiles are assumed white (true) by default
		if(tiles.has(location)) {
			tiles.set(location, !tiles.get(location));
		}
		else {
			tiles.set(location, false);
		}
	}

	// For part 1, just set 'turns' to 0.

	for(let i = 0; i < turns; i++) {
		const tiles_to_check = new Set();

		// First, generate all the tiles that need to be checked.
		// It can be shown that only tiles that are touching a black tile need to be checked.
		for(const [loc, value] of tiles.entries()) {
			if(value) {
				// Ignore white tiles
				continue;
			}

			const [x, y] = loc.split(' ').map((coord) => Number(coord));

			for(let dy = -1; dy <= 1; dy++) {
				if(dy !== 0) {
					tiles_to_check.add(`${x - 0.5} ${y + dy}`);
					tiles_to_check.add(`${x + 0.5} ${y + dy}`);
				}
				else {
					tiles_to_check.add(`${x - 1} ${y + dy}`);
					tiles_to_check.add(`${x + 1} ${y + dy}`);
				}
			}
			tiles_to_check.add(`${x} ${y}`);
		}

		// All the tiles are flipped at the same time, so maintain a list of changes instead of changing the map.
		const to_set = new Map();

		for(const loc of tiles_to_check) {
			const [x, y] = loc.split(' ').map((coord) => Number(coord));

			let black = 0;

			for(let dy = -1; dy <= 1; dy++) {
				let w, e;
				if(dy !== 0) {
					w = `${x - 0.5} ${y + dy}`;
					e = `${x + 0.5} ${y + dy}`;
				}
				else {
					w = `${x - 1} ${y + dy}`;
					e = `${x + 1} ${y + dy}`;
				}

				if(tiles.has(w) && !tiles.get(w)) {
					black++;
				}

				if(tiles.has(e) && !tiles.get(e)) {
					black++;
				}
			}

			// Tile was originally black
			if(tiles.has(loc) && !tiles.get(loc)) {
				if (black === 0 || black > 2) {
					to_set.set(loc, true);
				}
			}
			// Tile was originally white
			else {
				if(black === 2) {
					to_set.set(loc, false);
				}
			}
		}

		for(const [loc, value] of to_set) {
			tiles.set(loc, value);
		}
	}
	return num_black(tiles);
}

function num_black(tiles) {
	let black = 0;

	for(const [, value] of tiles.entries()) {
		if(!value) {
			black++;
		}
	}

	return black;
}

console.log(main());
