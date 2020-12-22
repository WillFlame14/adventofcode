'use strict';

const fs = require('fs');

// Pretty easy, but I tried storing a global set of all previous turns in all games and that made node run out of memory.
// Creating a new set for each game lets the garbage collector delete the sets from games that are already done.

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

	const boardstates = new Set();

	while(cards1.length !== 0 && cards2.length !== 0) {
		play(cards1, cards2, boardstates);
	}

	const winner_cards = cards1.length > 0 ? cards1 : cards2;

	let score = 0;

	for(let i = 0; i < winner_cards.length; i++) {
		score += winner_cards[winner_cards.length - 1 - i] * (i + 1);
	}

	return score;
}

function play(cards1, cards2, boardstates) {
	const state = cards1.join(', ') + '|' + cards2.join(', ');
	if(boardstates.has(state)) {
		// Force player 2 to lose immediately
		cards2.splice(0, cards2.length);
		return;
	}
	else {
		boardstates.add(state);
	}

	const card1 = cards1[0];
	const card2 = cards2[0];

	cards1.splice(0, 1);
	cards2.splice(0, 1);

	// Recursive combat
	if(card1 <= cards1.length && card2 <= cards2.length) {
		const new_cards1 = cards1.slice(0, card1);
		const new_cards2 = cards2.slice(0, card2);

		const new_boardstates = new Set();

		while(new_cards1.length !== 0 && new_cards2.length !== 0) {
			play(new_cards1, new_cards2, new_boardstates);
		}

		if(new_cards1.length !== 0) {
			cards1.push(card1);
			cards1.push(card2);

		}
		else {
			cards2.push(card2);
			cards2.push(card1);
		}
	}
	else {
		if(card1 > card2) {
			cards1.push(card1);
			cards1.push(card2);
		}
		else {
			cards2.push(card2);
			cards2.push(card1);
		}
	}
}

console.log(main());
