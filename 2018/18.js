import {getInputData} from './utils.js';

const data = (await getInputData(18)).trim().split('\n');

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

function calculateResourceValue(area) {
	let wooded = 0;
	let lumberyards = 0;
	for (const row of area) for (const cell of row) {
		if (cell === '|') wooded++;
		if (cell === '#') lumberyards++;
	}
	return wooded * lumberyards;
}

function solvePartOne() {
	const {area} = simulate(data, 10);
	return calculateResourceValue(area);
}

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