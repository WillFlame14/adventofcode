'use strict';

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let facing = 0;
	const dirs = ['E', 'S', 'W', 'N'];	// increment for cw, decrement for ccw

	const loc = { x: 0, y: 0 };

	for(const line of lines) {
		let dir = line.charAt(0);
		const dist = Number(line.substring(1));

		if(dir === 'F') {
			dir = dirs[facing];
		}

		switch(dir) {
			case 'N':
				loc.y += dist;
				break;
			case 'E':
				loc.x += dist;
				break;
			case 'S':
				loc.y -= dist;
				break;
			case 'W':
				loc.x -= dist;
				break;
			case 'L':
				facing -= dist / 90;
				while(facing < 0) {
					facing += 4;
				}
				break;
			case 'R':
				facing += dist / 90;
				while(facing > 3) {
					facing -= 4;
				}
				break;
		}
	}

	return Math.abs(loc.x) + Math.abs(loc.y);
}

console.log(main());
