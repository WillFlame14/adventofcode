'use strict';

const fs = require('fs');

function main() {
	const grid = [];
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	for(const line of lines) {
		grid.push(line.split(''));
	}

	const { portalToLocs, locsToPortals } = findAllPortals(grid);

	const x_extremes = [999, -1], y_extremes = [999, -1];

	// Finding the extreme x and y values of the portals to determine which are inside vs. outside
	for(const pairs of Object.values(portalToLocs)) {
		for(const pair of pairs) {
			if(pair[0] < x_extremes[0]) {
				x_extremes[0] = pair[0];
			}
			else if(pair[0] > x_extremes[1]) {
				x_extremes[1] = pair[0];
			}

			if(pair[1] < y_extremes[0]) {
				y_extremes[0] = pair[1];
			}
			else if(pair[1] > y_extremes[1]) {
				y_extremes[1] = pair[1];
			}
		}
	}

	// console.log(portalToLocs);
	// console.log(locsToPortals);
	// console.log(x_extremes + ' ' + y_extremes);

	const destination = portalToLocs['ZZ'][0];
	const queue = [{ loc: portalToLocs['AA'][0], steps: 0, path: ['AA 0'], level: 0 }];
	const visited = new Set();

	while(queue.length > 0) {
		const { loc, steps, path, level } = queue.shift();
		const current_loc = loc[0] + ' ' + loc[1];
		visited.add(current_loc + ' ' + level);

		if(loc[0] === destination[0] && loc[1] === destination[1]) {
			if(level > 0) {
				//console.log('found exit at level ' + level + ' with dist ' + steps);
				continue;
			}
			return steps;
		}
		else {
			// Try the 4 cardinal directions
			for(let x = -1; x <= 1; x++) {
				if(x + loc[0] < 0 || x + loc[0] >= grid.length) {
					continue;
				}
				for(let y = -1; y <= 1; y++) {
					if(loc[1] + y < 0 || loc[1] + y >= grid[loc[0]].length || Math.abs(x + y) !== 1) {
						continue;
					}

					const new_x = x + loc[0], new_y = y + loc[1];
					const new_loc = new_x + ' ' + new_y;
					if(visited.has(new_loc + ' ' + level) || grid[new_x][new_y] !== '.') {
						continue;
					}

					visited.add(new_loc + ' ' + level);
					queue.push({ loc: [new_x, new_y], steps: steps + 1, path: path.slice(), level });
				}
			}
			// See if there are any portals that can be used
			if(locsToPortals[current_loc] !== undefined) {
				const outside = x_extremes.includes(loc[0]) || y_extremes.includes(loc[1]);
				const new_level = level + (outside ? -1 : 1);

				const portal = locsToPortals[current_loc];

				// Cannot go to negative levels
				if(new_level < 0 && portal !== 'ZZ') {
					continue;
				}

				// AA doesn't count as a portal
				if(portal === 'AA') {
					continue;
				}

				for(const portal_loc of portalToLocs[portal]) {
					const new_loc2 = portal_loc[0] + ' ' + portal_loc[1];

					// Don't add the current portal
					if(current_loc === new_loc2) {
						continue;
					}

					if(visited.has(new_loc2 + ' ' + new_level)) {
						continue;
					}

					const new_path = path.slice();
					new_path.push(portal + ' ' + new_level);
					queue.push({ loc: portal_loc, steps: steps + 1, path: new_path, level: new_level });
				}
			}
		}
	}

	// console.log(best_path);

	// for(const loc of best_path) {
	// 	const [x, y] = loc.split(' ').map(val => Number(val));
	// 	grid[x][y] = 'x';
	// }
	// for(const line of grid) {
	// 	console.log(line.join(''));
	// }
	// console.log(best_path.length - 1);
}

function findAdjacent(grid, i, j) {
	const letters = [grid[i][j]];
	let portal_loc, other_letter_loc;

	for(let x = -1; x <= 1; x++) {
		if(x + i < 0 || x + i >= grid.length) {
			continue;
		}
		for(let y = -1; y <= 1; y++) {
			if(j + y < 0 || j + y >= grid[i].length || Math.abs(x + y) !== 1) {
				continue;
			}
			const char = grid[x + i][y + j];
			if('ABCDEFGHIJKLMNOPQRSTUVWXYZ'.includes(char)) {
				letters.push(char);
				other_letter_loc = [x + i, y + j];
				if(x < 0 || y < 0) {
					letters.reverse();
				}
			}
			else if(char === '.') {
				portal_loc = [x + i, y + j];
			}
		}
	}

	if(portal_loc === undefined) {
		portal_loc = [i + 2 * (other_letter_loc[0] - i), j + 2 * (other_letter_loc[1] - j)];
	}
	return { letters: letters.join(''), portal_loc, letter_locs: [[i, j], other_letter_loc] };
}

function findAllPortals(grid) {
	const portalToLocs = {}, locsToPortals = {};
	const letters_found = new Set();

	for(let i = 0; i < grid.length; i++) {
		for(let j = 0; j < grid[i].length; j++) {
			if('ABCDEFGHIJKLMNOPQRSTUVWXYZ'.includes(grid[i][j]) && !letters_found.has(i + ' ' + j)) {
				const { letters, portal_loc, letter_locs } = findAdjacent(grid, i, j);
				for(const loc of letter_locs) {
					letters_found.add(loc[0] + ' ' + loc[1]);
				}

				if(portalToLocs[letters] === undefined) {
					portalToLocs[letters] = [];
				}
				portalToLocs[letters].push(portal_loc);
				locsToPortals[portal_loc[0] + ' ' + portal_loc[1]] = letters;
			}
		}
	}
	return { portalToLocs, locsToPortals };
}

console.log(main());
