'use strict';

const fs = require('fs');

/* This one was hard. :( Spent a while trying to use Dijkstra's, but as I was storing each key as a node there was no way to verify all nodes have been visited.
 * Just ended up using brute force with aggressive caching. For part 2, notice that each robot only needs to find the shortest path to pick up all its keys.
 * Each robot can ignore all doors in its path, expecting another robot to pick up the keys eventually.
 */

const map = [];					// The actual map of the maze
const keys = new Map();			// Map of key --> { loc: { x: number, y: number }, keys_required: string[] }
const visited = new Set();		// Points that have been visited (used in the initial DFS)

const best = new Map();			// Map of keys collected --> shortest distance to collect rest of keys
const paths = new Map();		// Map of two keys --> shortest distance between them

async function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let start;

	for(const line of lines) {
		if(line.includes('@')) {
			start = { x: line.indexOf('@'), y: map.length };
			keys.set('@', { loc: start, keys_required: [] });
		}
		map.push(line.split(''));
	}

	visited.add(start.x + ' ' + start.y);

	// DFS through the entire maze
	await dfs(start, []);

	// Generate all possible ways to pick up all keys starting from here, and return the one with the lowest steps
	return traverse('@', 0, new Set());
}

/**
 * A basic depth-first search to find out where all the keys are and what doors need to be unlocked for each one
 * @param  {x: number, y: number} current_loc     The current location
 * @param  {string[]} 			  keys_required   Keys required to get to this location
 */
async function dfs(current_loc, keys_required) {
	const { x, y } = current_loc;
	const new_keys_required = keys_required.slice();

	// Check the 4 cardinal directions
	for(let i = -1; i <= 1; i++) {
		for(let j = -1; j <= 1; j++) {
			if(Math.abs(i + j) !== 1) {
				continue;
			}

			const char = map[y + j][x + i];
			const loc = { x: x + i, y: y + j };
			const loc_str = (x + i) + ' ' + (y + j);		// This location, but as a string so it can be used as a key for a map

			if(visited.has(loc_str) || char === '#') {
				continue;
			}

			visited.add(loc_str);

			if('abcdefghijklmnopqrstuvwxyz'.includes(char)) {
				// This tile is a key
				keys.set(char, { loc, keys_required: new_keys_required });
			}
			else if(char !== '.') {
				// This tile is a door - Comment the line below for part 2 to ignore doors
				new_keys_required.push(char.toLowerCase());
			}

			// DFS from this new location
			await dfs(loc, new_keys_required);
		}
	}
	return Promise.resolve();
}

/**
 * Gets the reachable keys, given a list of keys that have already been picked up.
 */
function get_reachable_keys(found_keys) {
	const reachable = [];
	for(const [key, value] of keys.entries()) {
		const { keys_required } = value;

		// Not enough keys found
		if(keys_required.length > found_keys.length) {
			continue;
		}

		let valid = true;
		for(const key_req of keys_required) {
			if(!found_keys.has(key_req)) {
				valid = false;
				break;
			}
		}

		if(valid) {
			reachable.push(key);
		}
	}
	return reachable;
}

/**
 * Uses breadth-first search to find the shortest path between two locations, given a set of keys that have already been picked up.
 * @param  {x: number, y: number} start       The starting location
 * @param  {x: number, y: number} destination The destination
 * @param  {Set<string>} 		  found_keys  Keys that have been picked up
 * @return {number}             			  The length of the shortest path, or -1 if it cannot be found.
 */
function find_path(start, destination, found_keys) {
	// Maintain a priority queue with locations to search next (ordered by increasing distance from destination)
	const queue = [{ loc: start, dist: 9999, traveled: 0 }];

	const visited2 = new Set();

	while(queue.length > 0) {
		const { loc, traveled } = queue.shift();
		const { x ,y } = loc;

		// Check the 4 cardinal directions
		for(let i = -1; i <= 1; i++) {
			for(let j = -1; j <= 1; j++) {
				if(Math.abs(i + j) !== 1) {
					continue;
				}

				const char = map[y + j][x + i];
				const new_loc = { x: x + i, y: y + j };
				const new_loc_str = (x + i) + ' ' + (y + j);
				const new_dist = Math.abs(destination.y - (y + j)) + Math.abs(destination.x - (x + i));		// Taxicab distance to destination

				if(visited2.has(new_loc_str) || char === '#') {
					continue;
				}

				// locked door - Comment the block below for part 2 to ignore doors
				if('ABCDEFGHIJKLMNOPQRSTUVWXYZ'.includes(char) && !found_keys.has(char.toLowerCase())) {
					continue;
				}

				visited2.add(new_loc_str);

				// Found the destination!
				if(new_dist === 0) {
					return traveled + 1;
				}

				// Insert this new node into the queue, but maintain the sorted order (by increasing distance)
				let index = 0;
				while(index < queue.length && queue[index].dist < new_dist) {
					index++;
				}
				queue.splice(index, 0, { loc: new_loc, dist: new_dist, traveled: traveled + 1 });
			}
		}
	}
	// Impossible to find path
	return -1;
}

/**
 * Recursive function that picks up all keys while maintaining the length of the path.
 * @param  {string} 	 key        The current key being picked up
 * @param  {number} 	 traveled   The distance traveled so far
 * @param  {Set<string>} found_keys The keys that have been picked up so far
 * @return {number}            		The minimum path length to pick up all the keys
 */
function traverse(key, traveled, found_keys) {
	found_keys.add(key);
	const reachable_keys = get_reachable_keys(found_keys).filter(new_key => !found_keys.has(new_key));

	if(reachable_keys.length === 0) {
		return traveled;
	}

	// Convert the keys that have been found into a string so it can be used as a key for a map
	const found_keys_hash = Array.from(found_keys).sort().join(' ') + ' ' + key;

	// If we've already been in this situation, we know how many steps remaining to pick up the rest of the keys.
	if(best.has(found_keys_hash)) {
		return traveled + best.get(found_keys_hash);
	}

	// Otherwise, let us search for the minimum path to pick up the remaining keys
	let min = Infinity;

	for(const new_key of reachable_keys) {
		if(found_keys.has(new_key)) {
			continue;
		}

		// Find the minimum distance from this key to to the reachable key.
		let dist;
		const path = key + ' ' + new_key;

		if(paths.has(path)) {
			dist = paths.get(path);
		}
		else {
			dist = find_path(keys.get(key).loc, keys.get(new_key).loc, found_keys);

			// Set both ways for convenience
			const path2 = new_key + ' ' + key;
			paths.set(path, dist);
			paths.set(path2, dist);
		}

		if(dist === -1) {
			throw new Error(`Cannot find path from ${key} to ${new_key}, with found_keys ${Array.from(found_keys.values())}.`);
		}

		const new_traveled = traveled + dist;
		const total_dist = traverse(new_key, new_traveled, new Set(found_keys));

		if(total_dist < min) {
			min = total_dist;
		}
	}

	// Save the shortest remaining distance given this hash
	best.set(found_keys_hash, min - traveled);

	return min;
}

main().then(val => {
	console.log(val);
});
