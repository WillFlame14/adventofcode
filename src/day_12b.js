'use strict';

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	const loc = { x: 0, y: 0 };
	const waypoint = { x: 10, y: 1 };		// This waypoint is always relative to the boat

	for(const line of lines) {
		const dir = line.charAt(0);
		const dist = Number(line.substring(1));

		if(dir === 'F') {
			loc.x += waypoint.x * dist;
			loc.y += waypoint.y * dist;
		}

		switch(dir) {
			case 'N':
				waypoint.y += dist;
				break;
			case 'E':
				waypoint.x += dist;
				break;
			case 'S':
				waypoint.y -= dist;
				break;
			case 'W':
				waypoint.x -= dist;
				break;
			case 'L':
				// This rotates 90 degrees ccw
				for(let i = 0; i < dist / 90; i++) {
					const save = waypoint.y;
					waypoint.y = waypoint.x;
					waypoint.x = -save;
				}
				break;
			case 'R':
				// This rotates 90 degrees cw
				for(let i = 0; i < dist / 90; i++) {
					const save = waypoint.y;
					waypoint.y = -waypoint.x;
					waypoint.x = save;
				}
				break;
		}
	}

	return Math.abs(loc.x) + Math.abs(loc.y);
}

console.log(main());
