'use strict';

let start, current_day;

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function updateCharts() {
	event.preventDefault();

	const raw_data = document.getElementById('jsonInput').value;
	try {
		const data = JSON.parse(raw_data);
		generate(data);

		document.getElementById('jsonParseError').style.display = 'none';
		document.getElementById('charts').style.display = 'flex';
	}
	catch(err) {
		document.getElementById('jsonParseError').style.display = 'block';
		document.getElementById('jsonParseError').innerHTML = err;
		throw err;
	}
}

const anon_users = {
	'991271': 'Tiger64guy',
	'1566620': 'Juandif'
};

function generate(data) {
	start = (Number(data.event) - 2020) * 86400 * 365 + 1606712400;			// Time when the first puzzle was released
	if (Number(data.event) >= 2020) {
		start += 86400;		// Leap day in 2020
	}
	current_day = Math.min(Math.floor((Date.now() / 1000 - start) / 86400) + 1, 25);		// The current day number

	// Global settings for all charts
	window.Chart.defaults.global.defaultFontColor = 'black';
	window.Chart.defaults.global.defaultFontFamily = 'Source Code Pro';
	window.Chart.defaults.global.defaultFontSize = 18;

	// Map of user --> some of their data
	const map = new Map();

	// Puzzle --> List of objects containing a user and their time for that puzzle
	const day_times = [...new Array(current_day * 2)].map(() => []);
	const day_elapsed = [...new Array(current_day)].map(() => []);

	Object.keys(data.members).forEach(id => {
		const member_data = data.members[id];
		const name = member_data.name || anon_users[id] || `user #${id}`;

		const timestamps = [];
		const elapsed = [];
		const stars = [{ x: start * 1000 }];

		Object.keys(member_data.completion_day_level).forEach(day => {
			const day_timestamps = member_data.completion_day_level[day];

			const first_star = ((day_timestamps[1].get_star_ts - (start + (day - 1) * 86400)) / 60).toFixed(2);
			timestamps[(day - 1) * 2] = first_star;
			day_times[(day - 1) * 2].push({ name, time: first_star });
			stars.push({ x: day_timestamps[1].get_star_ts * 1000 });

			if(day_timestamps[2] !== undefined) {
				const second_star = ((day_timestamps[2].get_star_ts - (start + (day - 1) * 86400)) / 60).toFixed(2);

				timestamps[(day - 1) * 2 + 1] = second_star;
				day_times[(day - 1) * 2 + 1].push({ name, time: second_star });
				stars.push({ x: day_timestamps[2].get_star_ts * 1000 });
				elapsed[day - 1] = (second_star - first_star).toFixed(2);
				day_elapsed[day - 1].push(second_star - first_star);
			}
		});

		// Only include users that have solved at least one puzzle
		if (stars.length > 1) {
			map.set(name, { id, timestamps, elapsed, points: [0], stars });
		}
	});

	const chart_ids = ['timestampChart1', 'timestampChart2', 'elapsedChart', 'pointsChart'];

	// Resize charts if there are too many series
	if (map.size > 15) {
		for (const id of chart_ids) {
			document.getElementById(id).height = 500 + 8 * map.size;
		}
	}

	const num_users = Object.keys(data.members).length;
	const median_elapsed = [];

	// Calculate scores
	day_times.forEach((puzzle_times, puzzle_index) => {
		// Ignore day 1
		if(puzzle_index < 2) {
			return;
		}

		// Sort the times for each star
		puzzle_times.sort((a, b) => a.time - b.time);

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

	// Calculate median for elapsed times
	day_elapsed.forEach((elapsed_times, puzzle_index) => {
		// Sort the times for each star
		elapsed_times.sort((a, b) => a - b);

		const middle = elapsed_times.length / 2;
		if(elapsed_times.length % 2 === 0) {
			median_elapsed[puzzle_index] = ((Number(elapsed_times[middle - 1]) + Number(elapsed_times[middle])) / 2).toFixed(2);
		}
		else {
			median_elapsed[puzzle_index] = elapsed_times[Math.floor(middle)].toFixed(2);
		}
	});

	const datasets = {};
	const colours = window.palette('tol-rainbow', map.size).map((hex) => ('#' + hex)).reverse();

	// For each member...
	Array.from(map.keys()).forEach((member, index) => {
		// Get all their datasets and add them to the compiled set
		Object.keys(map.get(member)).forEach(dataset => {
			const values = map.get(member)[dataset];

			if(datasets[dataset] === undefined) {
				datasets[dataset] = [];
			}

			// Generate the stars data properly
			if(dataset === 'stars') {
				values.sort((a, b) => a.x - b.x);
				values.forEach((point, index2) => {
					point.y = index2;
				});
			}

			const series = {
				label: member,
				data: values,
				lineTension: 0,
				backgroundColor: ['rgba(0, 0, 0, 0)'],
				borderColor: colours[index],
				pointBackgroundColor: colours[index],
				pointBorderWidth: 3,
				borderWidth: 2,
			};

			// Individual modifications
			switch(dataset) {
				case 'elapsed':
					series.showLine = false;
					break;
				case 'points':
				case 'stars':
					series.pointBorderWidth = 1;
			}

			datasets[dataset].push(series);
		});
	});

	// Add median dataset
	datasets['elapsed'].push({
		label: 'median',
		data: median_elapsed,
		lineTension: 0,
		backgroundColor: ['rgba(0, 0, 0, 0)'],
		borderColor: new Array(median_elapsed.length).fill('rgba(0, 0, 0, 1)'),
		pointBackgroundColor: new Array(median_elapsed.length).fill('rgba(0, 0, 0, 1)'),
		pointBorderWidth: 3,
		borderWidth: 2
	});

	// Generate labels for the graphs
	const labels = {
		timestamps: [],
		elapsed: [],
		points: ['0'],
		stars: [start * 1000]
	};

	for(let i = 1; i <= current_day; i++) {
		labels['timestamps'].push(`${i}.1`);
		labels['timestamps'].push(`${i}.2`);
		labels['elapsed'].push(`${i}`);
		labels['stars'].push((start + (i * 86400)) * 1000);

		if(i !== 1) {
			labels['points'].push(`${i}.1`);
			labels['points'].push(`${i}.2`);
		}
	}

	// Timestamp chart(2h)
	new window.Chart(document.getElementById('timestampChart1').getContext('2d'), {
		type: 'line',
		data: {
			labels: labels['timestamps'].slice(Math.max(0, labels['timestamps'].length - 10)),
			datasets: datasets['timestamps'].map(dataset => Object.assign({}, dataset, { data: dataset.data.slice(current_day * 2 - 10) }))
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
						max: 120,
						callback: function (value, index, _values) {
							return ['0', '5m', '10m', '30m', '1h', '2h'][index];
						}
					},
					afterBuildTicks: function (chartObj) {
						chartObj.ticks = [0, 5, 10, 30, 60, 120];
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
				position: 'right',
				labels: {
					filter: function(legendItem, chartData) {
						return chartData.datasets.some(dataset => dataset.label === legendItem.text && dataset.data.length > 0);
					}
				}
			},
			title: {
				display: true,
				text: 'Solve Time (first 2 hours)'
			}
		}
	});

	// Timestamp chart (24h)
	new window.Chart(document.getElementById('timestampChart2').getContext('2d'), {
		type: 'line',
		data: {
			labels: labels['timestamps'].slice(Math.max(0, labels['timestamps'].length - 10)),
			datasets: datasets['timestamps'].map(dataset => Object.assign({}, dataset, { data: dataset.data.slice(current_day * 2 - 10) }))
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
				position: 'right',
				labels: {
					filter: function(legendItem, chartData) {
						return chartData.datasets.some(dataset => dataset.label === legendItem.text && dataset.data.length > 0);
					}
				}
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
						max: 1440,
						callback: function (value, index, _values) {
							return ['0', '30s', '1m', '5m', '10m', '30m', '1h', '4h', '12h', '24h'][index];
						}
					},
					afterBuildTicks: function (chartObj) {
						chartObj.ticks = [0, 0.5, 1, 5, 10, 30, 60, 240, 720, 1440];
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
				position: 'right',
				onClick: function(e, legendItem) {
					const index = legendItem.datasetIndex;
					const ci = this.chart;

					ci.data.datasets[index].showLine = !ci.data.datasets[index].showLine;
					ci.update();
				},
				labels: {
					filter: function(legendItem, _chartData) {
						return legendItem.text !== 'median';
					}
				}
			},
			title: {
				display: true,
				text: 'Time Elapsed Between Parts 1 and 2 (+ median)'
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

	// Stars chart
	new window.Chart(document.getElementById('starsChart').getContext('2d'), {
		type: 'line',
		data: {
			labels: labels['stars'],
			datasets: datasets['stars']
		},
		options: {
			maintainAspectRatio: false,
			scales: {
				yAxes: [{
					scaleLabel: {
						display: 'true',
						labelString: 'Points'
					}
				}],
				xAxes: [{
					type: 'time',
					scaleLabel: {
						display: 'true',
						labelString: 'Time'
					},
					time: {
						unit: 'day'
					}
				}]
			},
			legend: {
				position: 'right'
			},
			title: {
				display: true,
				text: 'Stars Earned'
			}
		}
	});

	// Timestamp chart (all time)
	new window.Chart(document.getElementById('timestampChart3').getContext('2d'), {
		type: 'line',
		data: {
			labels: labels['timestamps'],
			datasets: datasets['timestamps']
		},
		options: {
			maintainAspectRatio: false,
			scales: {
				yAxes: [{
					type: 'logarithmic',
					scaleLabel: {
						display: 'true',
						labelString: 'Time'
					},
					ticks: {
						min: 0,
						max: 10080,
						callback: function (value, index, _values) {
							return ['0', '5m', '10m', '30m', '1h', '4h', '12h', '1d', '2d', '7d'][index];
						}
					},
					afterBuildTicks: function (chartObj) {
						chartObj.ticks = [0, 5, 10, 30, 60, 240, 720, 1440, 2880, 10080];
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
				text: 'Solve Time (all time)'
			}
		}
	});
}
