'use strict';

const fs = require('fs');

const chairs = [];		// Array of all the chair locations
const map = [];			// 2D array of the layout

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	for(const line of lines) {
		const row = [];
		for(let i = 0; i < line.length; i++) {
			const char = line.charAt(i);
			row.push(char);

			if(char === 'L') {
				chairs.push({ row: map.length, col: i, occ: false});
			}
		}
		map.push(row);
	}

	// Keep stepping while at least one chair changed
	while(step());

	// Count the number of occupied chairs
	return chairs.reduce((total, chair) => total += chair.occ ? 1 : 0, 0);
}

function step() {
	let changed = false;
	for(const loc of chairs) {
		const { col, row } = loc;
		let adj_occ = 0;	// Number of "adjacent" occupied chairs

		let done = false;		// Flag that indicates whether this chair has exhausted all directions
		const found = new Array(9).fill(false);		// State machine that indicates whether a chair has been found in a particular direction
		let i = 0;				// The current radius of the search

		while(!done) {
			done = true;		// Assume done unless found otherwise
			i++;		// Increment radius

			// Go through each of the 8 directions, verify that the location is valid, then check if a chair is there.
			// If there is a chair there, determine if it is occupied (change adj_occ accordingly), then set the state machine
			// so this direction no longer gets traveled in the future.
			// Note that a valid location means searching should be continued in this direction (assuming no chair has been found yet).
			if(col - i >= 0 && !found[1]) {
				if(map[row][col - i] !== '.') {
					adj_occ += map[row][col - i] === '#' ? 1 : 0;
					found[1] = true;
				}
				done = false;
			}

			if(col + i < map[0].length && !found[2]) {
				if(map[row][col + i] !== '.') {
					adj_occ += map[row][col + i] === '#' ? 1 : 0;
					found[2] = true;
				}
				done = false;
			}

			if(row - i >= 0 && !found[3]) {
				if(map[row - i][col] !== '.') {
					adj_occ += map[row - i][col] === '#' ? 1 : 0;
					found[3] = true;
				}
				done = false;
			}

			if(row + i < map.length && !found[4]) {
				if(map[row + i][col] !== '.') {
					adj_occ += map[row + i][col] === '#' ? 1 : 0;
					found[4] = true;
				}
				done = false;
			}

			if(row - i >= 0 && col - i >= 0 && !found[5]) {
				if(map[row - i][col - i] !== '.') {
					adj_occ += map[row - i][col - i] === '#' ? 1 : 0;
					found[5] = true;
				}
				done = false;
			}

			if(row - i >= 0 && col + i < map[0].length && !found[6]) {
				if(map[row - i][col + i] !== '.') {
					adj_occ += map[row - i][col + i] === '#' ? 1 : 0;
					found[6] = true;
				}
				done = false;
			}

			if(row + i < map.length && col - i >= 0 && !found[7]) {
				if(map[row + i][col - i] !== '.') {
					adj_occ += map[row + i][col - i] === '#' ? 1 : 0;
					found[7] = true;
				}
				done = false;
			}

			if(row + i < map.length && col + i < map[0].length && !found[8]) {
				if(map[row + i][col + i] !== '.') {
					adj_occ += map[row + i][col + i] === '#' ? 1 : 0;
					found[8] = true;
				}
				done = false;
			}
		}

		// Change chair occupancy based on occupancy of "adjacent" chairs
		// Note that the map does not get updated here.
		// This is because all chairs must be updated at the same time, so that is done afterward.
		if(loc.occ && adj_occ >= 5) {
			loc.occ = false;
			changed = true;
		}
		else if(!loc.occ && adj_occ === 0) {
			loc.occ = true;
			changed = true;
		}
	}

	// Update the map with the new chair occupancies.
	for(const loc of chairs) {
		const { col, row, occ } = loc;
		map[row][col] = occ ? '#' : 'L';
	}

	// Return whether a chair's occupancy was changed
	return changed;
}

console.log(main());
