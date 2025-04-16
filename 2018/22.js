import {getInputData} from './utils.js';

const data = (await getInputData(22)).trim().split('\n');
const depth = Number(data[0].split(': ')[1]);
const target = data[1].split(': ')[1].split(',').map(Number);

const neighbors = (x, y) => [[x - 1, y], [x + 1, y], [x, y - 1], [x, y + 1]];

const allowedTools = (erosionLevel) =>
	erosionLevel % 3 === 0 ? new Set([0, 1]) :
		erosionLevel % 3 === 1 ? new Set([1, 2]) :
			new Set([0, 2]);

function calculateRegionGrid() {
	const [targetX, targetY] = target;
	const maxX = targetX + 100;
	const maxY = targetY + 100;
	const grid = Array.from({length: maxY + 1}, () => Array(maxX + 1).fill(0));
	for (let y = 0; y <= maxY; y++) for (let x = 0; x <= maxX; x++) {
		let geoIndex;
		if ((x === 0 && y === 0) || (x === targetX && y === targetY)) geoIndex = 0;
		else if (y === 0) geoIndex = x * 16807;
		else if (x === 0) geoIndex = y * 48271;
		else geoIndex = grid[y - 1][x] * grid[y][x - 1];
		grid[y][x] = (geoIndex + depth) % 20183;
	}
	return grid;
}

function solvePartOne(grid) {
	const [targetX, targetY] = target;
	let totalRisk = 0;
	for (let y = 0; y <= targetY; y++) for (let x = 0; x <= targetX; x++) totalRisk += grid[y][x] % 3;
	return totalRisk;
}

function solvePartTwo(grid) {
	const [targetX, targetY] = target;
	const pq = [[0, 0, 0, 0]];
	const visited = new Set();
	while (pq.length > 0) {
		pq.sort((a, b) => a[0] - b[0]);
		const [time, x, y, tool] = pq.shift();
		const state = `${x},${y},${tool}`;
		if (visited.has(state)) continue;
		visited.add(state);
		if (x === targetX && y === targetY && tool === 0) return time;
		for (const [nx, ny] of neighbors(x, y)) {
			if (nx < 0 || ny < 0 || !grid[ny] || !grid[ny][nx]) continue;
			const fromTools = allowedTools(grid[y][x]);
			const toTools = allowedTools(grid[ny][nx]);
			const usableTools = [...fromTools].filter(t => toTools.has(t));
			if (usableTools.includes(tool)) pq.push([time + 1, nx, ny, tool]);
		}
		const currentRegionType = grid[y][x] % 3;
		for (const newTool of allowedTools(currentRegionType)) if (newTool !== tool) pq.push([time + 7, x, y, newTool]);
	}
	throw new Error('No path found');
}

const grid = calculateRegionGrid();
console.log(`Part One: ${solvePartOne(grid)}`);
console.log(`Part Two: ${solvePartTwo(grid)}`);