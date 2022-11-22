'use strict';

const canvas = document.getElementById('puzzleCanvas');
const ctx = canvas.getContext('2d');

const mid = 350;

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function drawPuzzle() {
	event.preventDefault();
	canvas.style.display = 'block';

	ctx.clearRect(0, 0, canvas.width, canvas.height);

	const lines = document.getElementById('puzzleInput').value.split(',').map(part => part.trim());
	console.log(lines);

	const carls = new Map();

	for(const line of lines) {
		const parts = line.split(' ');

		const id = parts[0];
		const carl = carls.has(id) ? carls.get(id) : { x: 0, y: 0, drawing: true, polar: false, facing: 0 };
		const operation = parts[1];

		if(carl.x === undefined) {
			console.log(line);
			console.log(carls.get(id));
		}

		switch(operation) {
			case 'stop':
				carl.drawing = false;
				break;
			case 'draw':
				carl.drawing = true;
				break;
			case 'polar':
				carl.polar = true;
				break;
			case 'cart':
				carl.polar = false;
				break;
			case 'left':
				carl.facing += Number(parts[2]);
				break;
			case 'right':
				carl.facing -= Number(parts[2]);
				break;
			case 'forward': {
				const distance = Number(parts[2]);
				const new_loc = {};

				new_loc.x = carl.x + distance * Math.cos(carl.facing * Math.PI / 180);
				new_loc.y = carl.y + distance * Math.sin(carl.facing * Math.PI / 180);

				if(carl.drawing) {
					drawLine(carl.x, carl.y, new_loc.x, new_loc.y);
				}
				carl.x = new_loc.x;
				carl.y = new_loc.y;
				break;
			}
			case 'move':
			case 'teleport': {
				const new_loc = get_new_loc(carl, parts);

				if(operation === 'move' && carl.drawing) {
					drawLine(carl.x, carl.y, new_loc.x, new_loc.y);
				}

				carl.x = new_loc.x;
				carl.y = new_loc.y;
				break;
			}
			case 'rotate': {
				let centre, angle;
				if(parts.length === 4) {
					centre = carls.get(parts[2]) || { x: 0, y: 0 };
					angle = Number(parts[3]);
				}
				else {
					centre = get_new_loc(carl, parts);
					angle = Number(parts[4]);
				}

				const radius = find_distance(centre.x, centre.y, carl.x, carl.y);
				const start_angle = Math.atan2(carl.y - centre.y, carl.x - centre.x);

				if(carl.drawing) {
					if(angle < 0) {
						drawArc(carl.x, carl.y, centre.x, centre.y, radius, start_angle + (angle * Math.PI / 180), start_angle);
					}
					else {
						drawArc(carl.x, carl.y, centre.x, centre.y, radius, start_angle, start_angle + (angle * Math.PI / 180));
					}
				}

				const new_coords = rotate(centre.x, centre.y, carl.x, carl.y, angle);
				carl.x = new_coords[0];
				carl.y = new_coords[1];
				break;
			}
		}

		carls.set(id, carl);
	}
}

function get_new_loc(carl, parts) {
	let new_loc;
	if(carl.polar) {
		const r = Number(parts[2]), theta = Number(parts[3]);
		new_loc = { x: r * Math.cos(theta * Math.PI / 180), y: r * Math.sin(theta * Math.PI / 180) };
	}
	else {
		new_loc = { x: Number(parts[2]), y: Number(parts[3]) };
	}
	return new_loc;
}

function rotate(cx, cy, x, y, angle) {
    const radians = (Math.PI / 180) * angle,
        cos = Math.cos(radians),
        sin = Math.sin(radians),
        nx = (cos * (x - cx)) + (sin * (y - cy)) + cx,
        ny = (cos * (y - cy)) - (sin * (x - cx)) + cy;
    return [nx, ny];
}

function find_distance(x1, y1, x2, y2) {
	console.log(x1, y1, x2, y2);
	console.log('dist', Math.sqrt(Math.pow(y2 - y1, 2) + Math.pow(x2 - x1, 2)));
	return Math.sqrt(Math.pow(y2 - y1, 2) + Math.pow(x2 - x1, 2));
}

function drawLine(sx, sy, fx, fy) {
	console.log('drawing', sx, sy, fx, fy);
	ctx.beginPath();
	ctx.moveTo(mid + sx, mid + sy);
	ctx.lineTo(mid + fx, mid + fy);
	ctx.stroke();
}

function drawArc(sx, sy, cx, cy, r, s_angle, f_angle) {
	console.log('rotating', sx, sy, cx, cy, r, s_angle, f_angle);
	ctx.beginPath();
	ctx.moveTo(mid + sx, mid + sy);
	ctx.arc(mid + cx, mid + cy, r, s_angle, f_angle);
	ctx.stroke();
}
