'use strict';

// Another opportunity to demonstrate my power over loops :)

/**
 * The basic idea is:
 *   - Determine if there are parentheses.
 *   - If so, find the smallest block enclosed by parentheses and calculate its result.
 *   - Replace the entire block with its result in the string.
 *   - Repeat until there are no more parentheses.
 *   - Calculate the result of the final expression.
 *
 * For part 1, simply replace all instances of calculate2() with calculate1().
 */

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let sum = 0;

	for(let line of lines) {
		while(line.indexOf('(') !== -1) {
			let open_index;

			for(let i = 0; i < line.length; i++) {
				const char = line.charAt(i);
				if(char === '(') {
					// Keep track of the latest open bracket
					open_index = i;
				}
				else if(char === ')') {
					// This is the first closing bracket, so it must enclose the smallest block
					// Snip out the contents and calculate their result
					const subresult = calculate2(line.substring(open_index + 1, i));

					// Then replace the block with the result
					line = line.substring(0, open_index) + subresult + line.substring(i + 1);

					// Break out of loop to check if expression still has parentheses
					break;
				}
			}
		}
		// Add result of current line to sum
		sum += calculate2(line);
	}

	return sum;
}

// Left-to-right order of operations
function calculate1(string) {
	const parts = string.split(' ');

	// Start with the leftmost number
	let result = Number(parts[0]);
	let operator;

	for(let i = 0; i < parts.length; i++) {
		const item = parts[i];

		// Not a number, so save it as the operator
		if(isNaN(Number(item))) {
			operator = item;
		}
		else {
			const operand = Number(item);

			switch(operator) {
				case '+':
					result += operand;
					break;
				case '*':
					result *= operand;
					break;
				default:
					// Do nothing. This can be reached if there's only a number inside parentheses, like '(23)'.
			}
		}
	}
	return result;
}

// Addition first, then multiplication
function calculate2(string) {
	// Cast all the numbers, leave the rest alone
	const parts = string.split(' ').map(part => Number(part) || part);

	// Add numbers together and replace their positions in the array until there are no more addition signs
	while(parts.includes('+')) {
		const index = parts.indexOf('+');
		parts[index - 1] = parts[index - 1] + parts[index + 1];
		parts.splice(index, 2);
	}

	// There are only multiplication symbols left, but calculate1() can do this for us anyways
	return calculate1(parts.join(' '));
}

console.log(main());
