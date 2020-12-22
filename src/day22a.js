'use strict';

const fs = require('fs');

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let player1 = true;

	const cards1 = [];
	const cards2 = [];

	for(const line of lines) {
		if(line === '') {
			player1 = false;
			continue;
		}

		if(line.includes('Player')) {
			continue;
		}

		if(player1) {
			cards1.push(Number(line));
		}
		else {
			cards2.push(Number(line));
		}
	}

	while(cards1.length !== 0 && cards2.length !== 0) {
		play(cards1, cards2);
	}

	const winner_cards = cards1.length > 0 ? cards1 : cards2;

	let score = 0;

	for(let i = 0; i < winner_cards.length; i++) {
		score += winner_cards[winner_cards.length - 1 - i] * (i + 1); 
	}

	return score;
}

function play(cards1, cards2) {
	// console.log(cards1, cards2);
	const card1 = cards1[0];
	const card2 = cards2[0];

	cards1.splice(0, 1);
	cards2.splice(0, 1);

	if(card1 > card2) {
		cards1.push(card1);
		cards1.push(card2);
	}
	else {
		cards2.push(card2);
		cards2.push(card1);
	}
}

console.log(main());
