'use strict';

const max = 1000000;
const turns = 10000000;

// Linked lists here to save the day. I originally thought that you'd still need to search
// for the destination cup, but keeping the cups in an array makes that an O(1) operation as well.

function main() {
	const raw_cups = '469217538'.split('');
	const cups = [];	// index is the cup number, value is the next clockwise cup

	// Set up the initial cups
	for(let i = 0; i < raw_cups.length; i++) {
		const value = Number(raw_cups[i]);

		cups[value] = Number(raw_cups[(i + 1) % max]) || 10;
	}

	let current = Number(raw_cups[0]);
	print(cups, current);

	// Add in the rest of the million cups
	for(let i = 10; i <= max; i++) {
		cups[i] = i === max ? current : i + 1;
	}

	// Go through the turns
	for(let i = 0; i < turns; i++) {
		let destination = current - 1;

		if(destination <= 0) {
			destination += max;
		}

		const pickup = [];
		let next_cup = current;

		for(let j = 0; j < 3; j++) {
			next_cup = cups[next_cup];
			pickup.push(next_cup);
		}

		// Join the ends
		cups[current] = cups[next_cup];

		while(pickup.includes(destination)) {
			destination--;

			// Wrap around
			if(destination === 0) {
				destination = max;
			}
		}

		// Thanks klyzx for this otherwise confusing bit
		cups[pickup[2]] = cups[destination];
		cups[destination] = pickup[0];

		current = cups[current];
	}

	return cups[1] * cups[cups[1]];
}

function print(cups, current) {
	const array = [current];
	const start = current;

	while(cups[current] !== start) {
		array.push(cups[current]);
		current = cups[current];
	}
	console.log(array.join(', '));
}

console.log(main());
