'use strict';

const fs = require('fs');

// Today is the day I get to demonstrate my power over loops 8)

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	// Locations of all currently active cells
	const cells = new Set();

	for(let i = 0; i < lines.length; i++) {
		const line = lines[i];
		for(let j = 0; j < line.length; j++) {
			const char = line.charAt(j);

			// Store the location as space-separated coordinates
			if(char === '#') {
				cells.add(`${i} ${j} 0 0`);
			}
		}
	}

	for(let turn = 0; turn < 6; turn++) {
		// The set containing all the coordinates we need to check for this turn
		const to_visit = new Set();

		// Find all cells that need to be checked
		for(const loc of cells) {
			const [x, y, z, w] = loc.split(' ').map((coord) => Number(coord));

			// Check all places within this n-dimensional cube
			for(let i = -1; i <= 1; i++) {
				for(let j = -1; j <= 1; j++) {
					for(let k = -1; k <= 1; k++) {
						for(let l = -1; l <= 1; l++) {
							// We can just add since 'visited' is a set, so duplicates are excluded.
							to_visit.add(`${x + i} ${y + j} ${z + k} ${w + l}`);
						}
					}
				}
			}
		}

		// These are separate so that the set of cells doesn't change in the middle of a turn.
		const to_delete = [], to_add = [];

		// Check the status of each cell
		for(const loc of to_visit) {
			const [x, y, z, w] = loc.split(' ').map((coord) => Number(coord));

			const active = cells.has(loc);
			let adj_active = 0;

			for(let i = -1; i <= 1; i++) {
				for(let j = -1; j <= 1; j++) {
					for(let k = -1; k <= 1; k++) {
						for(let l = -1; l <= 1; l++) {
							// Do not count the cell itself
							if(i === 0 && j === 0 && k === 0 && l === 0) {
								continue;
							}

							const new_loc = `${x + i} ${y + j} ${z + k} ${w + l}`;
							if(cells.has(new_loc)) {
								adj_active++;
							}
						}
					}
				}
			}

			// The set of cells only needs to be modified if there is a new activation or deletion (maintaining can be ignored)
			if(active && !(adj_active === 2 || adj_active === 3)) {
				to_delete.push(loc);
			}
			if(!active && adj_active === 3) {
				to_add.push(loc);
			}
		}

		// Add and delete cells fro this turn
		for(const loc of to_delete) {
			cells.delete(loc);
		}

		for(const loc of to_add) {
			cells.add(loc);
		}
	}

	return cells.size;
}

console.log(main());
