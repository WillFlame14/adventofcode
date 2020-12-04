'use strict';

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let num = 0;

	// Set of required keys
	const set = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'];

	let current_set = set.slice();
	let failed = false;

	for(const line of lines) {
		// Starting a new passport
		if(line === '') {
			// All keys met
			if(current_set.length === 0) {
				num++;
			}

			// Refill current_set with all the keys
			current_set = set.slice();
			failed = false;
		}

		// Short-circuit if this passport is already invalid
		if(failed) {
			continue;
		}

		const parts = line.split(' ');
		for(const part of parts) {
			const [key, val] = part.split(':');
			const index = current_set.indexOf(key);

			// This is a required key! Let's validate it.
			if(index !== -1) {
				switch(key) {
					case 'byr':
						if(Number(val) < 1920 || Number(val) > 2002) {
							failed = true;
						}
						break;
					case 'iyr':
						if(Number(val) < 2010 || Number(val) > 2020) {
							failed = true;
						}
						break;
					case 'eyr':
						if(Number(val) < 2020 || Number(val) > 2030) {
							failed = true;
						}
						break;
					case 'hgt': {
						const unit = val.substring(val.length - 2, val.length);
						const len = Number(val.substring(0, val.length - 2));

						if(isNaN(len)) {
							failed = true;
						}

						switch(unit) {
							case 'cm':
								if(len < 150 || len > 193) {
									failed = true;
								}
								break;
							case 'in':
								if(len < 59 || len > 76) {
									failed = true;
								}
								break;
							default:
								failed = true;
						}
						break;
					}
					case 'hcl':
						if(val.charAt(0) !== '#' || val.length !== 7) {
							failed = true;

						}

						// Make sure that the remaining characters are 0-9 or a-f
						for(let i = 1; i < val.length; i++) {
							if('0123456789abcdef'.indexOf(val.charAt(i)) === -1) {
								failed = true;
							}
						}
						break;
					case 'ecl':
						if(!['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'].includes(val)) {
							failed = true;
						}
						break;
					case 'pid':
						if(val.length !== 9 || isNaN(Number(val))) {
							failed = true;
						}
						break;
				}

				if(failed) {
					// Stop checking keys on this line
					break;
				}

				// Implicit else: remove this key from the list of required keys
				current_set.splice(index, 1);
			}
		}
	}
	return num;
}

console.log(main());
