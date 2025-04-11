import {getInputData} from './utils.js';

const data = (await getInputData(6)).split('\n').filter(x => x !== '').map(x => x.split(', ').map(Number));
const xMin = Math.min(...data.map(([x, _]) => x));
const xMax = Math.max(...data.map(([x, _]) => x));
const yMin = Math.min(...data.map(([_, y]) => y));
const yMax = Math.max(...data.map(([_, y]) => y));

const manhattanDistance = (x1, y1, x2, y2) => Math.abs(x1 - x2) + Math.abs(y1 - y2);

function solvePartOne() {
	let grid;
	grid = [];
	const areaCount = new Map();
	const infiniteAreas = new Set();
	for (let x = xMin; x <= xMax; x++) for (let y = yMin; y <= yMax; y++) {
		let minDistance = Infinity;
		let closestPoint = -1;
		let isTied = false;
		data.forEach(([px, py], index) => {
			const distance = manhattanDistance(x, y, px, py);
			if (distance < minDistance) {
				minDistance = distance;
				closestPoint = index;
				isTied = false;
			} else if (distance === minDistance) isTied = true;
		});
		if (!isTied) {
			grid.push({x, y, closestPoint});
			if (x === xMin || x === xMax || y === yMin || y === yMax) infiniteAreas.add(closestPoint);
			areaCount.set(closestPoint, (areaCount.get(closestPoint) || 0) + 1);
		}
	}
	let largestFiniteArea = 0;
	for (const [point, count] of areaCount.entries()) if (!infiniteAreas.has(point)) largestFiniteArea = Math.max(largestFiniteArea, count);
	return largestFiniteArea;
}

function solvePartTwo() {
	const regionSize = 10000;
	let safeRegionCount = 0;
	for (let x = xMin; x <= xMax; x++) for (let y = yMin; y <= yMax; y++) {
		const totalDistance = data.reduce((sum, [px, py]) => sum + manhattanDistance(x, y, px, py), 0);
		if (totalDistance < regionSize) safeRegionCount++;
	}
	return safeRegionCount;
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);