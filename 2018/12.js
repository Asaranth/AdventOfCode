import {getInputData} from './utils.js';

const data = (await getInputData(12)).trim().split('\n');
const initialState = data[0].replace('initial state: ', '');

function generateRecipe() {
	const recipe = new Set();
	for (const line of data.slice(1)) {
		const [pattern, result] = line.split(' => ');
		if (result === '#') recipe.add(pattern);
	}
	return recipe;
}

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

function solvePartOne() {
	const recipe = generateRecipe();
	let currentSet = new Set();
	for (let i = 0; i < initialState.length; i++) if (initialState[i] === '#') currentSet.add(i);
	for (let generation = 0; generation < 20; generation++) currentSet = nextGeneration(currentSet, recipe);
	return Array.from(currentSet).reduce((sum, index) => sum + index, 0);
}

function solvePartTwo() {
	const recipe = generateRecipe();
	let currentSet = new Set();
	for (let i = 0; i < initialState.length; i++) if (initialState[i] === '#') currentSet.add(i);
	let lastSum = 0;
	let growthRate = 0;
	let stableGenerationsCount = 0;
	const requiredStability = 10;
	const maxGenerationsToCheck = 2000; // Arbitrary safety limit
	const targetGeneration = 50000000000;
	for (let generation = 1; generation <= maxGenerationsToCheck; generation++) {
		currentSet = nextGeneration(currentSet, recipe);
		const currentSum = Array.from(currentSet).reduce((sum, index) => sum + index, 0);
		const currentGrowthRate = currentSum - lastSum;
		if (generation > 1) {
			if (currentGrowthRate === growthRate) stableGenerationsCount++;
			else {
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