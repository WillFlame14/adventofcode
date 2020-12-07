'use strict';

const fs = require('fs');

const visited = new Set();
const holds = new Map();
const heldBy = new Map();

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	for(const line of lines) {
		const parts = line.split('contain');

		const holder_parts = parts[0].trim().split(' ');
		const holder = holder_parts[0] + ' ' + holder_parts[1];

		const held_parts = parts[1].trim().split(', ');

		held_parts.forEach(held_part => {
			const parts2 = held_part.split(' ');

			if(!holds.has(holder)) {
				holds.set(holder, []);
			}

			if(parts2[0] === 'no') {
				holds.get(holder).push({ held: null, qty: 0 });
				return;
			}
			const qty = Number(parts2[0]);
			const held = parts2[1] + ' ' + parts2[2];

			if(!heldBy.has(held)) {
				heldBy.set(held, []);
			}

			holds.get(holder).push({ held, qty });
			heldBy.get(held).push({ holder, qty });
		});
	}

	// Part 1
	findHeldBy('shiny gold', heldBy);
	console.log(visited.size);

	// Part 2
	// For some reason there's 1 extra bag counted... not sure why.
	return findHolds('shiny gold', holds) - 1;
}

function findHeldBy(colour) {
	// This bag is not held by any other bag
	if(heldBy.get(colour) === undefined) {
		return;
	}

	// Go through the bags this is held by and if there are new ones, recurse through there
	heldBy.get(colour).forEach(bag => {
		if(!visited.has(bag.holder)) {
			visited.add(bag.holder);
			findHeldBy(bag.holder);
		}
	});
}

function findHolds(colour) {
	// This bag does not contain any other bags
	if(colour === null) {
		return 1;
	}

	// Go through the types of bags this bag holds, and find out how many bags those bags hold.
	// Multiply by the quantity of those types of bags. Start at 1 to include this bag.
	let sum = 1;
	holds.get(colour).forEach(bag => {
		sum += findHolds(bag.held) * bag.qty;
	});
	return sum;
}

console.log(main());
