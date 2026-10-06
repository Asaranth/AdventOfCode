/**
 * Day 18: Settlers of The North Pole.
 *
 * Simulates a 2D cellular automaton modelling forestry resources: open ground ('.'), trees ('|'), and lumberyards ('#').
 * State transitions per minute:
 * - Open ground ('.') becomes trees ('|') if surrounded by 3 or more tree acres.
 * - Trees ('|') become a lumberyard ('#') if surrounded by 3 or more lumberyard acres.
 * - A lumberyard ('#') remains a lumberyard if adjacent to at least 1 lumberyard and 1 tree acre; otherwise it becomes open ground ('.').
 * Part One computes the total resource value (wooded acres * lumberyard acres) after 10 minutes.
 * Part Two detects periodic cycles in state evolution to determine the resource value after 1,000,000,000 minutes.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(18)).trim().split('\n');

/**
 * Counts the occurrences of each acre type in the 8 adjacent neighbouring cells.
 *
 * @param {string[]} area - 2D grid represented as an array of strings
 * @param {number} x - Row index
 * @param {number} y - Column index
 * @returns {{'.': number, '|': number, '#': number}} Counts of adjacent open grounds, trees, and lumberyards
 */
function countAdjacent(area, x, y) {
	const directions = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]];
	const counts = {'.': 0, '|': 0, '#': 0};

	for (const [dx, dy] of directions) {
		const nx = x + dx;
		const ny = y + dy;
		if (nx >= 0 && ny >= 0 && nx < area.length && ny < area[0].length) counts[area[nx][ny]]++;
	}

	return counts;
}

/**
 * Computes the next state of the entire lumber collection area after 1 minute.
 *
 * @param {string[]} area - Current 2D grid state
 * @returns {string[]} New 2D grid state after 1 minute
 */
function nextState(area) {
	const newArea = [];

	for (let x = 0; x < area.length; x++) {
		const newRow = [];

		for (let y = 0; y < area[x].length; y++) {
			const current = area[x][y];
			const counts = countAdjacent(area, x, y);
			const trees = counts['|'];
			const lumberyards = counts['#'];

			if (current === '.' && trees >= 3) newRow.push('|');
			else if (current === '|' && lumberyards >= 3) newRow.push('#');
			else if (current === '#' && !(lumberyards >= 1 && trees >= 1)) newRow.push('.');
			else newRow.push(current);
		}

		newArea.push(newRow);
	}

	return newArea.map(row => row.join(''));
}

/**
 * Simulates the forest cellular automaton for up to `maxMinutes`, optionally detecting state repetition cycles.
 *
 * @param {string[]} area - Initial grid layout
 * @param {number} maxMinutes - Maximum minutes to simulate
 * @param {boolean} [detectCycle=false] - Whether to track repeated grid states and return cycle parameters
 * @returns {{area: string[], cycleStart?: number, cycleLength?: number}} Simulation result
 */
function simulate(area, maxMinutes, detectCycle = false) {
	if (!detectCycle) {
		for (let i = 0; i < maxMinutes; i++) area = nextState(area);
		return {area};
	}

	const seenStates = new Map();

	for (let minute = 0; minute <= maxMinutes; minute++) {
		const areaString = area.join('\n');

		if (seenStates.has(areaString)) {
			const cycleStart = seenStates.get(areaString);
			const cycleLength = minute - cycleStart;
			return {cycleStart, cycleLength, area};
		}

		seenStates.set(areaString, minute);
		area = nextState(area);
	}

	return {cycleStart: -1, cycleLength: -1};
}

/**
 * Calculates total resource value: wooded acres ('|') multiplied by lumberyards ('#').
 *
 * @param {string[]} area - Grid state
 * @returns {number} The resource value
 */
function calculateResourceValue(area) {
	let wooded = 0;
	let lumberyards = 0;

	for (const row of area) for (const cell of row) {
		if (cell === '|') wooded++;
		if (cell === '#') lumberyards++;
	}

	return wooded * lumberyards;
}

/**
 * Solves Part One: simulates 10 minutes and computes the total resource value.
 *
 * @returns {number} Total resource value after 10 minutes
 */
function solvePartOne() {
	const {area} = simulate(data, 10);
	return calculateResourceValue(area);
}

/**
 * Solves Part Two: detects cycle repetition and calculates the grid configuration at 1,000,000,000 minutes.
 *
 * @returns {number} Total resource value after 1,000,000,000 minutes
 */
function solvePartTwo() {
	const {cycleStart, cycleLength} = simulate(data, 1000, true);
	if (cycleStart === -1 || cycleLength === -1) throw new Error('Cycle not detected within the maximum simulation time!');

	const targetMinute = (1000000000 - cycleStart) % cycleLength + cycleStart;
	let area = data;

	for (let minute = 0; minute < targetMinute; minute++) area = nextState(area);

	return calculateResourceValue(area);
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);