'use strict';

const start = 1606798800;			// Time when the first puzzle was released
const current_day = Math.floor((Date.now() / 1000 - start) / 86400) + 1;		// The current day number

const colours = [
	'rgba(225, 129, 92, 1)',
	'rgba(80, 80, 60, 1)',
	'rgba(155, 156, 86, 1)',
	'rgba(55, 122, 182, 1)',
	'rgba(173, 92, 205, 1)',
	'rgba(205, 159, 64, 1)',
	'rgba(54, 172, 195, 1)',
	'rgba(185, 48, 21, 1)',
	'rgba(65, 149, 132, 1)',
	'rgba(0, 50, 85, 1)',
	'rgba(65, 160, 23, 1)',
	'rgba(115, 69, 142, 1)',
	'rgba(94, 112, 165, 1)',
	'rgba(45, 55, 74, 1)'
];

// eslint-disable-next-line no-unused-vars, @typescript-eslint/no-unused-vars
function updateCharts() {
	event.preventDefault();

	const raw_data = document.getElementById('jsonInput').value;
	try {
		const data = JSON.parse(raw_data);
		generate(data);

		document.getElementById('jsonParseError').style.display = 'none';
	}
	catch(err) {
		document.getElementById('jsonParseError').style.display = 'block';
		document.getElementById('jsonParseError').innerHTML = err;
		throw err;
	}
}

const anon_users = {
	'991271': 'Tiger64guy'
};

function generate(data) {
	// Global settings for all charts
	window.Chart.defaults.global.defaultFontColor = 'black';
	window.Chart.defaults.global.defaultFontFamily = 'Source Code Pro';
	window.Chart.defaults.global.defaultFontSize = 18;

	// Map of user --> some of their data
	const map = new Map();

	// Puzzle --> List of objects containing a user and their time for that puzzle
	const day_times = [...new Array(current_day * 2 + 2)].map(() => []);

	Object.keys(data.members).forEach(id => {
		const member_data = data.members[id];
		const name = member_data.name || anon_users[id] || `user #${id}`;

		const timestamps = [];
		const elapsed = [];

		Object.keys(member_data.completion_day_level).forEach(day => {
			const day_timestamps = member_data.completion_day_level[day];

			const first_star = ((day_timestamps[1].get_star_ts - (start + (day - 1) * 86400)) / 60).toFixed(2);
			timestamps[(day - 1) * 2] = first_star;
			day_times[(day - 1) * 2].push({ name, time: first_star });

			if(day_timestamps[2] !== undefined) {
				const second_star = ((day_timestamps[2].get_star_ts - (start + (day - 1) * 86400)) / 60).toFixed(2);

				timestamps[(day - 1) * 2 + 1] = second_star;
				day_times[(day - 1) * 2 + 1].push({ name, time: second_star });
				elapsed[day - 1] = (second_star - first_star).toFixed(2);
			}
		});

		map.set(name, { id, timestamps, elapsed, points: [0] });
	});

	const num_users = Object.keys(data.members).length;

	day_times.forEach((puzzle_times, puzzle_index) => {
		// Ignore day 1
		if(puzzle_index < 2) {
			return;
		}

		// Sort the times for each star
		puzzle_times.sort((a, b) => {
			return a.time - b.time;
		});

		const solved_users = new Set();

		puzzle_times.forEach((score, index) => {
			const { points } = map.get(score.name);
			points.push(points[points.length - 1] + (num_users - index));

			solved_users.add(score.name);
		});

		Array.from(map.keys()).forEach(name => {
			// User did not solve this puzzle
			if(!solved_users.has(name)) {
				const { points } = map.get(name);
				points.push(points[points.length - 1]);
			}
		});
	});

	const datasets = {};

	// For each member...
	Array.from(map.keys()).forEach((member, index) => {
		// Get all their datasets and add them to the compiled set
		Object.keys(map.get(member)).forEach(dataset => {
			const values = map.get(member)[dataset];

			if(datasets[dataset] === undefined) {
				datasets[dataset] = [];
			}

			datasets[dataset].push({
				label: member,
				data: values,
				lineTension: 0,
				backgroundColor: ['rgba(0, 0, 0, 0)'],
				borderColor: new Array(values.length).fill(colours[index]),
				pointBackgroundColor: new Array(values.length).fill(colours[index]),
				pointBorderWidth: 3,
				borderWidth: 2
			});
		});
	});

	const labels = {
		'timestamps': [],
		'elapsed': [],
		'points': ['0']
	};

	for(let i = 1; i <= current_day; i++) {
		labels['timestamps'].push(`${i}.1`);
		labels['timestamps'].push(`${i}.2`);
		labels['elapsed'].push(`${i}`);

		if(i !== 1) {
			labels['points'].push(`${i}.1`);
			labels['points'].push(`${i}.2`);
		}
	}

	// Timestamp chart(1h)
	new window.Chart(document.getElementById('timestampChart1').getContext('2d'), {
		type: 'line',
		data: {
			labels: labels['timestamps'],
			datasets: datasets['timestamps']
		},
		options: {
			responsive: false,
			scales: {
				yAxes: [{
					type: 'logarithmic',
					scaleLabel: {
						display: 'true',
						labelString: 'Time'
					},
					ticks: {
						min: 0,
						max: 60,
						callback: function (value, index, _values) {
							return ['0', '5m', '10m', '30m', '1h'][index];
						}
					},
					afterBuildTicks: function (chartObj) {
						chartObj.ticks = [0, 5, 10, 30, 60];
					}
				}],
				xAxes: [{
					scaleLabel: {
						display: 'true',
						labelString: 'Day'
					}
				}]
			},
			legend: {
				position: 'right'
			},
			title: {
				display: true,
				text: 'Solve Time (first hour)'
			}
		}
	});

	// Timestamp chart (24h)
	new window.Chart(document.getElementById('timestampChart2').getContext('2d'), {
		type: 'line',
		data: {
			labels: labels['timestamps'],
			datasets: datasets['timestamps']
		},
		options: {
			responsive: false,
			scales: {
				yAxes: [{
					type: 'logarithmic',
					scaleLabel: {
						display: 'true',
						labelString: 'Time'
					},
					ticks: {
						min: 0,
						max: 1440,
						callback: function (value, index, _values) {
							return ['0', '5m', '10m', '30m', '1h', '4h', '12h', '24h'][index];
						}
					},
					afterBuildTicks: function (chartObj) {
						chartObj.ticks = [0, 5, 10, 30, 60, 240, 720, 1440];
					}
				}],
				xAxes: [{
					scaleLabel: {
						display: 'true',
						labelString: 'Day'
					}
				}]
			},
			legend: {
				position: 'right'
			},
			title: {
				display: true,
				text: 'Solve Time (first day)'
			}
		}
	});

	// Elapsed Time chart
	new window.Chart(document.getElementById('elapsedChart').getContext('2d'), {
		type: 'line',
		data: {
			labels: labels['elapsed'],
			datasets: datasets['elapsed']
		},
		options: {
			responsive: false,
			scales: {
				yAxes: [{
					type: 'logarithmic',
					scaleLabel: {
						display: 'true',
						labelString: 'Time'
					},
					ticks: {
						min: 0,
						max: 240,
						callback: function (value, index, _values) {
							return ['0', '30s', '1m', '5m', '10m', '30m', '1h', '4h'][index];
						}
					},
					afterBuildTicks: function (chartObj) {
						chartObj.ticks = [0, 0.5, 1, 5, 10, 30, 60, 240];
					}
				}],
				xAxes: [{
					scaleLabel: {
						display: 'true',
						labelString: 'Day'
					}
				}]
			},
			legend: {
				position: 'right'
			},
			title: {
				display: true,
				text: 'Time Elapsed Between Parts 1 and 2'
			}
		}
	});

	// Points chart
	new window.Chart(document.getElementById('pointsChart').getContext('2d'), {
		type: 'line',
		data: {
			labels: labels['points'],
			datasets: datasets['points']
		},
		options: {
			responsive: false,
			scales: {
				yAxes: [{
					scaleLabel: {
						display: 'true',
						labelString: 'Points'
					}
				}],
				xAxes: [{
					scaleLabel: {
						display: 'true',
						labelString: 'Day'
					}
				}]
			},
			legend: {
				position: 'right'
			},
			title: {
				display: true,
				text: 'Leaderboard Score'
			}
		}
	});
}
