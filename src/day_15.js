'use strict';

// This brute force solution works, but can take up to 10 minutes to run.
// I coded a C++ solution as well (same algorithm) that runs much faster. why is JS bad

function main() {
	const input = [2,1,10,11,0,6];

	const memory = [];
	let dist = 0, num;

	for(let i = 0; i < 30000000; i++) {
		// A progress bar since this is *really* slow
		if(i % 100000 === 0) {
			console.log(i);
		}
		num = i < input.length ? input[i] : dist;

		if(memory[num] !== undefined) {
			dist = i - memory[num];
		}
		else {
			dist = 0;
		}
		memory[num] = i;
	}

	return num;
}

console.log(main());
