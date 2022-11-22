'use strict';

const fs = require('fs');

const queue = [];

function part1() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let ans = 0;

	for(const line of lines) {
		const num = Number(line);

		if(queue.length >= 50) {
			// Check if two numbers in the previous 50 sum to the current one
			let found = false;
			for(let i = queue.length - 51; i <= queue.length; i++) {
				for(let j = i + 1; j <= queue.length; j++) {
					if(queue[i] + queue[j] === num && i !== j) {
						found = true;
						break;
					}
				}
				if(found) {
					break;
				}
			}
			if(!found) {
				ans = num;
				break;
			}
		}
		queue.push(num);
	}

	return ans;
}

function part2() {
	const ans = part1();
	console.log(ans);

	// Indexes of the sliding window, both inclusive
	let left = 0, right = 0;
	let sum = queue[0];

	// Move sliding window until the sum is correct
	while(sum !== ans) {
		if(sum > ans) {
			sum -= queue[left];
			left++;
		}
		if(sum < ans) {
			right++;
			sum += queue[right];
		}
	}

	let min = Infinity, max = -1;

	// Find max and min (easier to do afterwards so 2nd max and 2nd min don't need to be maintained)
	for(let i = left; i <= right; i++) {
		if(queue[i] > max) {
			max = queue[i];
		}
		if(queue[i] < min) {
			min = queue[i];
		}
	}
	return min + max;
}

console.log(part2());
