'use strict';

const fs = require('fs');

const ings_no_allergen = new Set();
const appearances = new Map();
const map = new Map();

function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	for(const line of lines) {
		const parts = line.split('(');
		const ingredients = parts[0].trim().split(' ');
		const allergens = parts[1].substring(9, parts[1].length - 1).split(', ');

		for(const ing of ingredients) {
			ings_no_allergen.add(ing);

			if(!appearances.has(ing)) {
				appearances.set(ing, 0);
			}
			appearances.set(ing, appearances.get(ing) + 1);
		}


		for(const allergen of allergens) {
			if(!map.has(allergen)) {
				// Set "possible ingredients that this allergen could be found in"
				map.set(allergen, ingredients.slice());
			}
			else {
				// Check against previous ingredients
				let previous_ings = map.get(allergen);
				for(let i = 0; i < previous_ings.length; i++) {
					if(!ingredients.includes(previous_ings[i])) {
						previous_ings[i] = null;
					}
				}

				previous_ings = previous_ings.filter(ing => ing !== null);
				map.set(allergen, previous_ings);
			}
		}
	}

	let change = true;
	while(change) {
		change = false;
		for(const [allergen, possible_ings] of map.entries()) {
			if(possible_ings.length === 1) {
				const ingredient = possible_ings[0];
				ings_no_allergen.delete(ingredient);

				for(const [allergen2, possible_ings2] of map.entries()) {
					if(allergen2 !== allergen && possible_ings2.includes(ingredient)) {
						possible_ings2.splice(possible_ings2.indexOf(ingredient), 1);
						change = true;
					}
				}
			}
		}
	}

	let total_appearances = 0;

	for(const ing of ings_no_allergen) {
		total_appearances += appearances.get(ing);
	}

	// Part 1
	console.log(total_appearances);

	const dangerous_ings = [];
	for(const [allergen, ings] of map.entries()) {
		dangerous_ings.push({ ing: ings[0], allergen });
	}

	// Sort alphabetically by allergen
	dangerous_ings.sort((a, b) => (a.allergen < b.allergen ? -1 : 1));

	return dangerous_ings.map(ing => ing.ing).join(",");
}

console.log(main());
