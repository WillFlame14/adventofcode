'use strict';

function main() {
	const card_pk = 14205034;
	const door_pk = 18047856;

	let card_ls = null, door_ls = null;
	let loop_size = 2, val = 7;

	while(card_ls === null || door_ls === null) {
		val = (val * 7) % 20201227;

		if(card_pk === val) {
			card_ls = loop_size;
		}
		if(door_pk === val) {
			door_ls = loop_size;
		}
		loop_size++;
	}

	let key = card_pk;

	for(let i = 1; i < door_ls; i++) {
		key = (key * card_pk) % 20201227;
	}
	return key;
}

console.log(main());
