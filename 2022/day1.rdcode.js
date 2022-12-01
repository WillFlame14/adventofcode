const fs = require('fs');

// Converts the puzzle input into VFX events.
function main() {
	const parts = fs.readFileSync('./input.txt').toString().split('\n');

	let bar = 11, beat = 1;

	for (const line of parts) {
		if (beat === 1 || beat === 11 || beat === 21 || beat === 31 || beat === 41) {
			console.log(`{ "bar": ${bar}, "beat": ${beat}, "y": 2, "type": "TagAction", "Action": "Run", "Tag": "run_sol" },`);
			console.log(`{ "bar": ${bar}, "beat": ${beat}, "y": 3, "type": "TagAction", "Action": "Run", "Tag": "run_sol2" },`);
		}

		if (line !== "") {
			console.log(`{ "bar": ${bar}, "beat": ${beat}, "y": 0, "type": "CallCustomMethod", "methodName": "i0 = ${line}", "executionTime": "OnBar", "sortOffset": 0 },`)
		}

		beat++;

		if (beat === 51) {
			beat = 1;
			bar++;
		}
	}
}

main();
