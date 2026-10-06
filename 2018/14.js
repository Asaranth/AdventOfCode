/**
 * Day 14: Chocolate Charts.
 *
 * Simulates a recipe generation algorithm where two elves create new recipes by combining their current ones.
 * The digits of the sum of their recipe scores are appended to the scoreboard.
 * Elves then advance (1 + score) spaces forward circularly.
 * Part One finds the scores of the 10 recipes immediately following the input recipe count.
 * Part Two finds how many recipes appear to the left of the input score sequence before it first occurs.
 */

import {getInputData} from './utils.js';

const data = Number(await getInputData(14));

/**
 * Creates new recipes by adding the current recipe scores of both elves and appends the resulting digits.
 * Advances the elves' positions forward in the scoreboard array.
 *
 * @param {number[]} scoreboard - Array of recipe scores
 * @param {number} elf1 - Current index of elf 1
 * @param {number} elf2 - Current index of elf 2
 * @returns {{scoreboard: number[], elf1: number, elf2: number}} Updated scoreboard and elf positions
 */
function generateRecipes(scoreboard, elf1, elf2) {
	const newRecipeSum = scoreboard[elf1] + scoreboard[elf2];
	const newRecipes = newRecipeSum.toString().split('').map(Number);
	scoreboard.push(...newRecipes);

	elf1 = (elf1 + 1 + scoreboard[elf1]) % scoreboard.length;
	elf2 = (elf2 + 1 + scoreboard[elf2]) % scoreboard.length;

	return {scoreboard, elf1, elf2};
}

/**
 * Checks whether the target sequence of digits matches the tail of the scoreboard.
 * Accounts for 1 or 2 newly added digits per iteration step.
 *
 * @param {number[]} scoreboard - Current recipe scores
 * @param {string} targetSequence - The target string pattern to locate
 * @returns {number|null} The starting index of the target sequence if found, or null otherwise
 */
function checkTargetSequence(scoreboard, targetSequence) {
	const targetLength = targetSequence.length;

	if (scoreboard.slice(-targetLength).join('') === targetSequence) return scoreboard.length - targetLength;
	if (scoreboard.slice(-(targetLength + 1), -1).join('') === targetSequence) return scoreboard.length - targetLength - 1;

	return null;
}

/**
 * Solves Part One: generates recipes until reaching data + 10 recipes,
 * and extracts the 10 scores following the initial `data` recipe count.
 *
 * @returns {string} The string of 10 recipe scores
 */
function solvePartOne() {
	let scoreboard = [3, 7], elf1 = 0, elf2 = 1;

	while (scoreboard.length < data + 10) {
		({scoreboard, elf1, elf2} = generateRecipes(scoreboard, elf1, elf2));
	}

	return scoreboard.slice(data, data + 10).join('');
}

/**
 * Solves Part Two: generates recipes until the target digit sequence is first encountered,
 * returning the count of recipes preceding it.
 *
 * @returns {number} The number of recipes created to the left of the target sequence
 */
function solvePartTwo() {
	let scoreboard = [3, 7], elf1 = 0, elf2 = 1;
	const targetSequence = data.toString();

	while (true) {
		({scoreboard, elf1, elf2} = generateRecipes(scoreboard, elf1, elf2));
		const sequenceMatchIndex = checkTargetSequence(scoreboard, targetSequence);
		if (sequenceMatchIndex !== null) return sequenceMatchIndex;
	}
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);