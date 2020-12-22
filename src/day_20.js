'use strict';

const fs = require('fs');

/**
 * The difficulty spike continues. Spent a really long time trying to get rotation/invert propagation working.
 * (i.e. If you know how two pieces are connected if one has no transformations applied, connect them again after transformations are applied.)
 * What I'm doing now is kinda hacky but I think it's quite intuitive, even if there's repeated work being done.
 *
 * For some reason part 1 doesn't work anymore... but I'm too tired to figure out why.
 */

// NOTE: I always order the sides by top, right, bottom, left, reading in clockwise order.
// (e.g. sides[1] is the rightmost column, top to bottom, while sides[2] is the bottom row, right to left.)

let map = [];

const map_sides = new Map();		// map of id --> list of sides
const map_tiles = new Map();		// map of id --> the tile, oriented as given in the input

const connections = new Map();		// map of id --> connected tiles
const used_tiles = new Set();

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let id_input = true, id;
	let tile = [];

	let sides = ["", "", "", ""];		// top, right, bottom, left

	for(const line of lines) {
		// Just finished a tile
		if(line === "") {
			map_sides.set(id, sides);
			map_tiles.set(id, tile);

			// Reset values
			sides = ["", "", "", ""];
			tile = [];

			id_input = true;
			continue;
		}

		// Reading the tile id
		if(id_input) {
			id = Number(line.split(' ')[1].slice(0, -1));
			id_input = false;
			continue;
		}

		// First line in the tile
		if(tile.length === 0) {
			sides[0] = line;
		}
		// Last line in the tile
		else if(tile.length === 9) {
			sides[2] = line.split("").reverse().join("");
		}

		// Create the left and right sides as we go along
		sides[1] += line.charAt(9);
		sides[3] = line.charAt(0) + sides[3];

		// Also create the tile
		tile.push(line.split(""));
	}

	// Find all connections. 'own_side' is the index of the connecting side on the original tile, 'other_side' is for the other tile.
	for([id, sides] of map_sides) {
		connections.set(id, []);

		// For each side of this tile...
		for(let i = 0; i < sides.length; i++) {
			const side = sides[i];
			let found = false;

			// Go through all other sides
			for(const [id2, sides2] of map_sides.entries()) {
				// Don't include the same time
				if(id2 === id) {
					continue;
				}

				for(let j = 0; j < sides2.length; j++) {
					if(side === sides2[j]) {
						connections.get(id).push({ tile_id: id2, own_side: i, other_side: j });
						found = true;
						break;
					}

					const inverted_side = sides2[j].split("").reverse().join("");

					if(side === inverted_side) {
						connections.get(id).push({ tile_id: id2, own_side: i, other_side: j });
						found = true;
						break;
					}
				}

				if(found) {
					break;
				}
			}
		}
	}

	let ans = 1;

	for(const [k, v] of connections.entries()) {
		// If there are only 2 connections, must be a corner tile
		if(v.length === 2) {
			ans *= k;
		}
	}

	// Just draw any tile that exists, doesn't matter where. The others will be drawn in DFS
	draw(1223).then(() => {
		print_tile(map);

		// Remove all the empty space
		map = simplify_map(map);

		let roughness = find_roughness(map);

		// Rotate map to find roughness
		if(roughness === 0) {
			for(let i = 0; i < 3; i++) {
				map = rotate(map);
				roughness = find_roughness(map);
				if(roughness !== 0) {
					break;
				}
			}
		}

		// Invert then rotate map to find roughness
		if(roughness === 0) {
			map = invert_horizontal(rotate(map));
			roughness = find_roughness(map);

			if(roughness === 0) {
				for(let i = 0; i < 3; i++) {
					map = rotate(map);
					roughness = find_roughness(map);

					if(roughness !== 0) {
						break;
					}
				}
			}
		}
		// Final answer for part 2
		console.log(roughness);
	});

	return ans;
}

/**
 * Draws one tile onto the map, and recursively calls draw() on its connected tiles that haven't already been drawn.
 * We start with x and y at 144 so there is room to travel in all directions.
 * @param  {Number}  id        	The current tile id
 * @param  {Number}  x         	The top-left x-coordinate of this tile
 * @param  {Number}  y         	The top-left y-coordinate of this tile (note: increasing y goes downward, not up)
 * @param  {Number}  rotations 	The number of times this tile must be rotated clockwise
 * @param  {Boolean} inverted  	Whether this tile should be inverted
 * @param  {Boolean} vertical  	Whether the inversion should be vertical or not (does nothing if inverted is false).
 * @return {Promise<void>}      Lets the program know when all tiles have been drawn.
 */
async function draw(id, x = 144, y = 144, rotations = 0, inverted = false, vertical = false) {
	let tile = map_tiles.get(id);
	used_tiles.add(id);

	for(let i = 0; i < rotations; i++) {
		tile = rotate(tile);
	}

	if(inverted) {
		if(vertical) {
			tile = invert_vertical(tile);
		}
		else {
			tile = invert_horizontal(tile);
		}
	}

	// Add the tile to the map
	for(let i = 1; i < tile.length - 1; i++) {
		if(map[y + i] === undefined) {
			map[y + i] = [];
		}
		for(let j = 1; j < tile[i].length - 1; j++) {
			map[y + i][x + j] = tile[i][j];
		}
	}

	const connected_tiles = connections.get(id);

	for(const connected_tile of connected_tiles) {
		// eslint-disable-next-line prefer-const
		let { tile_id, own_side, other_side } = connected_tile;

		// Do not draw tiles that have already been drawn
		if(used_tiles.has(tile_id)) {
			continue;
		}

		// What the connecting side was before any rotations were applied
		const old_connection = map_sides.get(id)[own_side];
		const old_connection_inv = old_connection.split("").reverse().join("");

		const new_sides = get_sides(tile);
		let connection_side, new_connection;

		// Find where the connection went, and whether it was inverted or not
		for(let i = 0; i < 4; i++) {
			if(new_sides[i] === old_connection) {
				connection_side = i;
				new_connection = old_connection;
				break;
			}
			else if(new_sides[i] === old_connection_inv) {
				connection_side = i;
				new_connection = old_connection_inv;
				break;
			}
		}

		let new_x = x, new_y = y;

		// 'connection_side' is the side of the original tile that connects to the new one.
		// This determines where the next tile should be drawn.
		if(connection_side % 4 === 0) {
			new_y -= 11;
		}
		else if(connection_side % 4 === 1) {
			new_x += 11;
		}
		else if(connection_side % 4 === 2) {
			new_y += 11;
		}
		else {
			new_x -= 11;
		}

		// Rotations to get the next tile's side aligned with the current tile's side
		const new_rotations = (4 + (connection_side + 2) - other_side) % 4;

		// What the connection is on the other tile (no rotations)
		const old_connection_other = map_sides.get(tile_id)[other_side];

		// If it's a direct match, it's actually supposed to be inverted
		const new_inverted = old_connection_other === new_connection;
		const new_vertical = (connection_side % 2) === 1;

		await draw(tile_id, new_x, new_y, new_rotations, new_inverted, new_vertical);
	}
	return Promise.resolve();
}

const monster = [
	'                  # ',
	'#    ##    ##    ###',
	' #  #  #  #  #  #   '
];

function find_roughness(curr_map) {
	// The final map, with all the monsters located on it
	let duplicate = JSON.parse(JSON.stringify(curr_map));

	// The tester, used to see what the map would look like if there were monsters found on it
	let tester = JSON.parse(JSON.stringify(curr_map));
	let monsters = 0;

	// Go through all possible 3x20 rectangles
	for(let i = 0; i < curr_map.length - 2; i++) {
		for(let j = 0; j < curr_map[i].length - 19; j++) {
			let failed = false;
			tester = JSON.parse(JSON.stringify(duplicate));

			// Compare this rectangle with the monster pattern
			for(let row = 0; row < 3; row++) {
				for(let col = 0; col < 20; col++) {
					if(monster[row][col] === '#') {
						if(curr_map[i + row][j + col] === '.') {
							failed = true;
							break;
						}
						else {
							tester[i + row][j + col] = 'o';
						}
					}
				}
				if(failed) {
					break;
				}
			}
			if(!failed) {
				monsters++;
				duplicate = JSON.parse(JSON.stringify(tester));
			}
		}
	}

	let roughness = 0;

	// If there are any monsters, calculate the roughness
	if(monsters > 0) {
		for(let i = 0; i < duplicate.length; i++) {
			for(let j = 0; j < duplicate[i].length; j++) {
				if(duplicate[i][j] === '#') {
					roughness++;
				}
			}
		}
	}
	return roughness;
}

/**
 * Returns the sides of a tile.
 */
function get_sides(tile) {
	const sides = ['', '', '', ''];

	sides[0] = tile[0].join("");
	sides[2] = tile[tile.length - 1].slice().reverse().join("");
	for(let i = 0; i < tile.length; i++) {
		const row = tile[i];
		sides[1] += row[9];
		sides[3] = row[0] + sides[3];
	}
	return sides;
}

/**
 * Rotates an array 90 degrees clockwise.
 */
function rotate(array) {
	const result = [];
	for(let j = 0; j < array[0].length; j++) {
		const row = [];
		for(let i = array.length - 1; i >= 0; i--) {
			row.push(array[i][j]);
		}
		result.push(row);
	}
	return result;
}

/**
 * Vertically flips an array.
 */
function invert_vertical(array) {
	const result = [];
	for(let i = array.length - 1; i >= 0; i--) {
		const row = [];
		for(let j = 0; j < array[0].length; j++) {
			row.push(array[i][j]);
		}
		result.push(row);
	}
	return result;
}

/**
 * Horizontally flips an array.
 */
function invert_horizontal(array) {
	const result = [];
	for(let i = 0; i < array.length; i++) {
		const row = [];
		for(let j = array[0].length - 1; j >= 0; j--) {
			row.push(array[i][j]);
		}
		result.push(row);
	}
	return result;
}

/**
 * Removes all the empty spaces in the map.
 */
function simplify_map(_map) {
	const new_map = [];
	for(let i = 0; i < _map.length; i++) {
		if(_map[i] === undefined) {
			continue;
		}
		new_map.push(_map[i].join("").split(""));
	}
	return new_map;
}

/**
 * Neatly prints out a tile. Mostly used for debugging.
 */
function print_tile(tile) {
	for(let i = 0; i < tile.length; i++) {
		if(tile[i] === undefined) {
			continue;
		}
		console.log((tile[i]).join(""));
	}
}

console.log(main());
