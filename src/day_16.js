'use strict';

const fs = require('fs');

function main1() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let stage = 0;
	const rules = {};
	let num = 0;

	for(const line of lines) {
		if(line === '') {
			stage++;
		}

		// Reading the rules
		if(stage === 0) {
			const parts = line.split(':');
			const rule = parts[0];

			const values = parts[1].trim().split(' ');
			const [min1, max1] = values[0].split('-').map(val => Number(val));
			const [min2, max2] = values[2].split('-').map(val => Number(val));

			// Use a function that returns true if the test number is valid
			rules[rule] = (test) => (test >= min1 && test <= max1) || (test >= min2 && test <= max2);
		}
		// Reading your own ticket
		else if(stage === 1) {
			// Do nothing
		}
		// Reading nearby tickets
		else {
			if(line === 'nearby tickets:') {
				continue;
			}
			const parts = line.split(',').map(val => Number(val));

			for(const val of parts) {
				let valid = false;
				for(const [, test] of Object.entries(rules)) {
					if(test(val)) {
						valid = true;
						break;
					}
				}
				// This value is not valid for any rules
				if(!valid) {
					num += val;
				}
			}
		}
	}

	return num;
}

function main2() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let stage = 0;
	const rules = {};
	let num = 1;
	let your_ticket;

	const allowed = [];

	for(const line of lines) {
		if(line === '') {
			stage++;
		}

		// Reading the rules
		if(stage === 0) {
			const parts = line.split(':');
			const rule = parts[0];

			const values = parts[1].trim().split(' ');
			const [min1, max1] = values[0].split('-').map(val => Number(val));
			const [min2, max2] = values[2].split('-').map(val => Number(val));

			// Use a function that returns true if the test number is valid
			rules[rule] = (test) => (test >= min1 && test <= max1) || (test >= min2 && test <= max2);
		}
		// Reading your own ticket
		else if(stage === 1) {
			if(line === 'your ticket:') {
				continue;
			}
			your_ticket = line.split(',').map(val => Number(val));
		}
		// Reading nearby tickets
		else {
			if(line === 'nearby tickets:') {
				continue;
			}
			const parts = line.split(',').map(val => Number(val));

			// For the current ticket, index --> valid rules for the number at this index
			const current = [];
			let valid = true;

			for(let i = 0; i < parts.length; i++) {
				const val = parts[i];

				let pass = false;
				for(const [rule, test] of Object.entries(rules)) {
					if(test(val)) {
						pass = true;
						if(current[i] === undefined) {
							current[i] = [];
						}
						// Add this rule to the current index
						current[i].push(rule);
					}
				}
				// This value is not valid for any rules (i.e. invalid ticket, ignore)
				if(!pass) {
					valid = false;
					break;
				}
			}

			// Only update global rules if this ticket is valid
			if(valid) {
				current.forEach((obj, index) => {
					// Assign the current allowed rules and use as a base if there are none yet
					if(allowed[index] === undefined) {
						allowed[index] = obj;
					}
					else {
						// Otherwise, compare currently allowed to previously allowed and only keep rules that are in both
						for(const rule of allowed[index]) {
							if(!obj.includes(rule)) {
								allowed[index].splice(allowed[index].indexOf(rule), 1);
							}
						}
					}
				});
			}
		}
	}

	// Rules that have been assigned to an index
	const found = new Set();

	while(found.size !== allowed.length) {
		// Find the new singleton
		let singleton;
		for(let i = 0; i < allowed.length; i++) {
			if(allowed[i].length === 1 && !found.has(allowed[i][0])) {
				found.add(allowed[i][0]);
				singleton = allowed[i][0];
			}
		}

		// Delete singleton from all other indexes
		for(let i = 0; i < allowed.length; i++) {
			const allowed_rules = allowed[i];
			const index = allowed_rules.indexOf(singleton);
			if(allowed_rules.length > 1 && index !== -1) {
				allowed_rules.splice(index, 1);
			}
		}
	}

	// Find answer
	for(let i = 0; i < allowed.length; i++) {
		if(allowed[i][0].includes('departure')) {
			num *= your_ticket[i];
		}
	}

	return num;
}

console.log(main1());
console.log(main2());
