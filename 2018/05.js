/**
 * Day 05: Alchemical Reduction.
 *
 * Simulates polymer reactions where adjacent units of the same type and opposite polarities (case) destroy each other.
 * Part One reduces the full polymer string using a stack and returns the resulting length.
 * Part Two tests removing all instances of each alphabet unit (case-insensitive) to find the shortest possible reacted polymer length.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(5)).trim();

/**
 * Simulates the alchemical reduction of a polymer string using a stack-based cancellation approach.
 * Adjacent characters that represent the same letter with differing case (e.g. 'aA' or 'Bb') annihilate each other.
 *
 * @param {string} str - The polymer string to react
 * @returns {string} The fully reacted polymer string
 */
function reactPolymer(str) {
	const stack = [];

	for (const char of str) {
		const lastChar = stack[stack.length - 1];

		if (lastChar && lastChar.toLowerCase() === char.toLowerCase() && lastChar !== char) {
			stack.pop();
		} else {
			stack.push(char);
		}
	}

	return stack.join('');
}

/**
 * Solves Part Two: iterates through all 26 letters of the alphabet, removes all occurrences
 * of each letter from the polymer, reacts the remaining polymer, and returns the minimum length found.
 *
 * @returns {number} The shortest reacted polymer length achievable by removing one unit type
 */
function solvePartTwo() {
	const alphabet = 'abcdefghijklmnopqrstuvwxyz';
	let shortestLength = Infinity;

	for (const char of alphabet) {
		const filteredPolymer = data.replace(new RegExp(char, 'gi'), '');
		const reactedPolymer = reactPolymer(filteredPolymer);

		shortestLength = Math.min(shortestLength, reactedPolymer.length);
	}

	return shortestLength;
}

console.log(`Part One: ${reactPolymer(data).length}`);
console.log(`Part Two: ${solvePartTwo()}`);