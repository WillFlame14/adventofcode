'use strict';

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let num = 0;
	let ids = [];
	let new_group = true;

	for(const line of lines) {
		if(line === '') {
			num += ids.length;
			ids = [];
			new_group = true;
			continue;
		}

		if(new_group) {
			// Add all the unique answers to the list
			for(let i = 0; i < line.length; i++) {
				if(!ids.includes(line.charAt(i))) {
					ids.push(line.charAt(i));
				}
			}
			new_group = false;
		}
		else {
			// Check all existing answers with this current line's answers
			// Array element is set to null because deleting elements while iterating through array is bad
			for(let i = 0; i < ids.length; i++) {
				if(!line.includes(ids[i])) {
					ids[i] = null;
				}
			}
			// Delete all the null elements
			ids = ids.filter(id => id !== null);
		}
	}

	return num;
}

console.log(main());
