'use strict';

const start = 1606798800;		// Time when the first puzzle was released

const colours = [
	'rgba(205, 99, 132, 1)',
	'rgba(54, 162, 185, 1)',
	'rgba(155, 156, 86, 1)',
	'rgba(55, 122, 182, 1)',
	'rgba(173, 92, 205, 1)',
	'rgba(205, 159, 64, 1)',
	'rgba(165, 149, 132, 1)',
	'rgba(94, 112, 165, 1)',
	'rgba(65, 149, 132, 1)',
	'rgba(0, 50, 85, 1)',
	'rgba(85, 120, 93, 1)',
	'rgba(145, 90, 13, 1)',
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
	}
}

function generate(data) {
	// Global settings for all charts
	window.Chart.defaults.global.defaultFontColor = 'black';
	window.Chart.defaults.global.defaultFontFamily = 'Source Code Pro';
	window.Chart.defaults.global.defaultFontSize = 18;

	// Map of user --> some of their data
	const map = new Map();

	Object.keys(data.members).forEach(id => {
		const member_data = data.members[id];

		const timestamps = [];
		const elapsed = [];

		Object.keys(member_data.completion_day_level).forEach((day, index) => {
			const day_timestamps = member_data.completion_day_level[index + 1];

			const first_star = ((day_timestamps[1].get_star_ts - (start + index * 86400)) / 60).toFixed(2);
			const second_star = ((day_timestamps[2].get_star_ts - (start + index * 86400)) / 60).toFixed(2);

			timestamps.push(first_star);
			timestamps.push(second_star);

			elapsed.push((second_star - first_star).toFixed(2));
		});
		map.set(member_data.name || 'Tiger64guy', { id, timestamps, elapsed });
	});

	const timestamp_datasets = [];
	const elapsed_datasets = [];

	Array.from(map.keys()).forEach((member, index) => {
		const base_dataset = {
			label: member,
			backgroundColor: ['rgba(0, 0, 0, 0)'],
			borderColor: [colours[index], colours[index]],
			pointBackgroundColor: [colours[index], colours[index]],
			pointBorderWidth: 3,
			borderWidth: 2
		};

		timestamp_datasets.push(Object.assign({ data: map.get(member).timestamps }, base_dataset));
		elapsed_datasets.push(Object.assign({ data: map.get(member).elapsed }, base_dataset));
	});

	console.log(timestamp_datasets);
	console.log(elapsed_datasets);

	// Timestamp chart
	new window.Chart(document.getElementById('timestampChart').getContext('2d'), {
		type: 'line',
		data: {
			labels: ['1.1', '1.2', '2.1', '2.2'],
			datasets: timestamp_datasets
		},
		options: {
			responsive: false,
			scales: {
				yAxes: [{
					type: 'logarithmic',
					scaleLabel: {
						display: 'true',
						labelString: 'Time (minutes)'
					},
					ticks: {
						min: 5, //minimum tick
						max: 1000, //maximum tick
						callback: function (value, index, values) {
							return Number(value.toString());//pass tick values as a string into Number function
						}
					},
					afterBuildTicks: function (chartObj) { //Build ticks labelling as per your need
						chartObj.ticks = [];
						// chartObj.ticks.push(0.1);
						chartObj.ticks.push(5);
						chartObj.ticks.push(10);
						chartObj.ticks.push(100);
						chartObj.ticks.push(1000);
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
				text: 'Solve Time'
			}
		}
	});

	// Elapsed Time chart
	new window.Chart(document.getElementById('elapsedChart').getContext('2d'), {
		type: 'line',
		data: {
			labels: ['1', '2'],
			datasets: elapsed_datasets
		},
		options: {
			responsive: false,
			scales: {
				yAxes: [{
					scaleLabel: {
						display: 'true',
						labelString: 'Time (minutes)'
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
}
