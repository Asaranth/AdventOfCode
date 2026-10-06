/**
 * Day 12: Subterranean Sustainability.
 *
 * Simulates a 1D cellular automaton of plants in pots governed by 5-pot neighbourhood rules.
 * Part One runs the simulation for 20 generations and sums all indices containing plants.
 * Part Two detects when the generation sum transitions into a linear growth pattern (constant derivative)
 * to extrapolate the sum after 50,000,000,000 generations.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(12)).trim().split('\n');
const initialState = data[0].replace('initial state: ', '');

/**
 * Parses the mutation rules that produce a live plant ('#') from a 5-pot pattern.
 *
 * @returns {Set<string>} Set of 5-character patterns that produce a plant ('#')
 */
function generateRecipe() {
	const recipe = new Set();
	for (const line of data.slice(1)) {
		const [pattern, result] = line.split(' => ');
		if (result === '#') recipe.add(pattern);
	}
	return recipe;
}

/**
 * Computes the state of alive plant indices for the next generation.
 *
 * @param {Set<number>} currentSet - Set of pot indices currently containing plants
 * @param {Set<string>} recipe - Set of 5-character patterns yielding a live plant
 * @returns {Set<number>} Set of pot indices containing plants in the next generation
 */
function nextGeneration(currentSet, recipe) {
	const start = Math.min(...currentSet) - 3;
	const end = Math.max(...currentSet) + 3;
	const nextSet = new Set();

	for (let i = start; i <= end; i++) {
		const pattern = [-2, -1, 0, 1, 2].map(offset => currentSet.has(i + offset) ? '#' : '.').join('');
		if (recipe.has(pattern)) nextSet.add(i);
	}

	return nextSet;
}

/**
 * Solves Part One: simulates 20 generations and sums all pot numbers containing a plant.
 *
 * @returns {number} Sum of plant pot indices after 20 generations
 */
function solvePartOne() {
	const recipe = generateRecipe();
	let currentSet = new Set();

	for (let i = 0; i < initialState.length; i++) if (initialState[i] === '#') currentSet.add(i);

	for (let generation = 0; generation < 20; generation++) currentSet = nextGeneration(currentSet, recipe);

	return Array.from(currentSet).reduce((sum, index) => sum + index, 0);
}

/**
 * Solves Part Two: monitors generation-by-generation sum changes until the growth rate
 * stabilises to a constant value, then linearly extrapolates the result to 50 billion generations.
 *
 * @returns {number} Extrapolated sum of plant pot indices after 50,000,000,000 generations
 */
function solvePartTwo() {
	const recipe = generateRecipe();
	let currentSet = new Set();
	for (let i = 0; i < initialState.length; i++) if (initialState[i] === '#') currentSet.add(i);

	let lastSum = 0;
	let growthRate = 0;
	let stableGenerationsCount = 0;
	const requiredStability = 10;
	const maxGenerationsToCheck = 2000;
	const targetGeneration = 50000000000;

	for (let generation = 1; generation <= maxGenerationsToCheck; generation++) {
		currentSet = nextGeneration(currentSet, recipe);
		const currentSum = Array.from(currentSet).reduce((sum, index) => sum + index, 0);
		const currentGrowthRate = currentSum - lastSum;

		if (generation > 1) {
			if (currentGrowthRate === growthRate) {
				stableGenerationsCount++;
			} else {
				stableGenerationsCount = 0;
				growthRate = currentGrowthRate;
			}

			if (stableGenerationsCount >= requiredStability) {
				const generationsRemaining = targetGeneration - generation;
				return currentSum + generationsRemaining * growthRate;
			}
		}

		lastSum = currentSum;
	}

	return Array.from(currentSet).reduce((sum, index) => sum + index, 0);
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);