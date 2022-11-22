'use strict';

const fs = require('fs');

// Ah yes, here's the difficulty spike. This was fun but also a little frustrating to debug.

const tests = [];
const ruleset = new Map();
const cache = new Map();

async function main() {
	const lines = fs.readFileSync('input.txt').toString().split('\r\n');

	let sum = 0;
	let reading_input = true;	// whether the input is currently being parsed or not

	for(const line of lines) {
		if(line === '') {
			reading_input = false;
			continue;
		}

		if(reading_input) {
			const parts = line.split(' ');
			const rule_num = parts[0].substring(0, parts[0].length - 1);	// Misleading since I'm storing as a string, but w/e
			const rules = [];

			/**
			 * Rules are stored like [['4', '5'], ['2', '3']]. (This means '4 5 | 2 3'.)
			 * Other rules are stored like [['4']] or ['a'].
			 */

			if(parts.includes('|')) {
				const index = parts.indexOf('|');

				rules.push(parts.slice(1, index));
				rules.push(parts.slice(index + 1));
			}
			else {
				const rule = parts.slice(1);

				if(rule.length === 1 && rule[0].includes('"')) {
					rules.push(rule[0].charAt(1));
				}
				else {
					rules.push(rule);
				}
			}
			ruleset.set(rule_num, rules);
		}
		else {
			// Add tests to a list
			tests.push(line);
		}
	}

	// 0 is the rule at the root, so this guarantees all rules will be found
	await find_rule('0');

	const cache42 = cache.get("42");
	const cache31 = cache.get("31");

	// I checked that cache42 and cache31 have no overlapping elements. thank

	/**
	 * General idea for Part 2:
	 * - For each string, delete any valid string for rule 42 from the beginning.
	 * - If deleted strings >= 1, then delete any valid string for rule 31 from the beginning.
	 * - If deleted strings for rule 31 < deleted strings for rule 42 and the string is now empty, increment counter.
	 *
	 * This works because rules 8 and 11 only appear for rule 0, and:
	 *  - rule 8 can be reduced to "rule 42 repeatedly, as needed"
	 *  - rule 11 can be reduced to "sandwich self in between rule 42 and rule 31 repeatedly, as needed"
	 *  Thus, we can see that any valid string will have rule 42 appear N times (N > 0) followed by rule 31 appearing M times (M > 0, M < N).
	 *  	e.g. 42 42 42 31 31 is valid because it can be grouped like 42 (42 (42 31) 31), which is 8 followed by 11 sandwiched inside itself
	 *
	 * For Part 1, either compare all test strings to cache0 or force rule 42 to be found twice and rule 31 to be found once.
	 * 		i.e. 8 11, or 42 (42 31)
	 */

	for(let test of tests) {
		let found_42 = true;
		let found_42_attempts = 0;

		while(found_42) {
			found_42 = false;
			for(const str of cache42) {
				if(test.startsWith(str)) {
					found_42 = true;
					test = test.substring(str.length);
					found_42_attempts++;
					break;
				}
			}
		}

		if(found_42_attempts === 0) {
			// Need at least one instance of 42
			continue;
		}

		let found_31 = true;
		let found_31_attempts = 0;

		while(found_31) {
			found_31 = false;
			for(const str of cache31) {
				if(test.startsWith(str)) {
					found_31 = true;
					test = test.substring(str.length);
					found_31_attempts++;
					break;
				}
			}
		}

		// Need at least one instance of 31, but it needs to be less than the instances of 42.
		// The entire test string should also be consumed.
		if(found_31_attempts > 0 && found_31_attempts < found_42_attempts && test.length === 0) {
			sum++;
		}
	}

	return sum;
}

/**
 * Recursively find all allowed strings for a certain rule.
 * Promises are used to make sure the cache can only be accessed after all rules are defined.
 * It also ensures that dependent rules finish first, allowing full usage of memoization.
 */
async function find_rule (rule_num) {
	// The rules for this have already been found
	if(cache.has(rule_num)) {
		return cache.get(rule_num);
	}
	// This rule is just a character
	else if(isNaN(Number(rule_num))) {
		return [rule_num];
	}

	const rules = ruleset.get(rule_num);
	let allowed = [];

	for(const rule of rules) {
		// List of currently allowed rules.
		let rule_allowed = [""];

		// A subrule is one specific rule number
		for(const subrule of rule) {
			// These are all the suffixes that the subrule allows (can be >= 1)
			const allowed_suffixes = await find_rule(subrule);
			const new_allowed = [];

			// Append all allowed suffixes to all previously allowed rules to get all allowed strings
			for(const str of rule_allowed) {
				for(const suffix of allowed_suffixes) {
					new_allowed.push(str + suffix);
				}
			}
			rule_allowed = new_allowed;
		}
		// Connect the rules with a pipe (potentially)
		allowed = allowed.concat(rule_allowed);
	}

	// All the allowed strings for this rule have been found, memoize it
	cache.set(rule_num, allowed);
	return Promise.resolve(allowed);
}

main().then(val => {
	console.log(val);
});

