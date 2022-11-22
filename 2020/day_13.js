'use strict';

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	const start = Number(lines[0]);
	const buses = lines[1].split(",");

	let min = Infinity, min_bus;

	buses.forEach(bus => {
		if(bus !== 'x') {
			const wait = Number(bus) - start % Number(bus);
			if(wait < min) {
				min = wait;
				min_bus = bus;
			}
		}
	});

	console.log(min, min_bus);

	return min * min_bus;
}

/**
 * I spent a really long time trying to figure out some way to solve a system of equations, but I basically know nothing about modular arithmetic.
 * I ended up plugging the numbers into a Chinese Remainder Theorem calculator, but I thought the idea below (from r/adventofcode) was very ingenious.
 * It relies on the observation that if x satisfies x mod a = p and x mod b = q, then x + lcm(a, b) also satisfies the two properties.
 * This lets you constantly increase the size of your step, making brute force much faster.
 * Also, thankfully all the bus values are prime so no fancy LCM calculations need to be done.
 */

function main2() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	const buses = lines[1].split(",").map(bus => Number(bus));

	let attempt = buses[0];
	let lcm = buses[0];

	for(let i = 1; i < buses.length; i++) {
		const bus = buses[i];

		if(isNaN(bus)) {
			continue;
		}
		while((attempt + i) % bus !== 0) {
			attempt += lcm;
		}

		lcm *= bus;
	}

	return attempt;
}


console.log(main2());
