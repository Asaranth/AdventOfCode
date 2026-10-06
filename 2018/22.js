/**
 * Day 22: Mode Maze.
 *
 * Models a cave system with three region types determined by geological index and erosion level:
 * - Rocky (risk 0, erosion % 3 === 0): Climbing gear (1) or Torch (0)
 * - Wet (risk 1, erosion % 3 === 1): Climbing gear (1) or Neither (2)
 * - Narrow (risk 2, erosion % 3 === 2): Torch (0) or Neither (2)
 * Tools: Torch = 0, Climbing Gear = 1, Neither = 2.
 * Part One computes the total risk level of the rectangular area from (0,0) to the target.
 * Part Two finds the fastest path to the target (holding the torch) using Dijkstra's shortest path algorithm.
 * Moving to an adjacent region takes 1 minute; switching tools takes 7 minutes.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(22)).trim().split('\n');
const depth = Number(data[0].split(': ')[1]);
const target = data[1].split(': ')[1].split(',').map(Number);

/**
 * Returns the 4 orthogonal neighbour coordinates.
 *
 * @param {number} x - X coordinate
 * @param {number} y - Y coordinate
 * @returns {Array<[number, number]>} Array of [x, y] coordinates
 */
const neighbors = (x, y) => [[x - 1, y], [x + 1, y], [x, y - 1], [x, y + 1]];

/**
 * Determines which tools are valid in a given region type based on its erosion level.
 * Tools: 0 = Torch, 1 = Climbing Gear, 2 = Neither.
 *
 * @param {number} erosionLevel - Region erosion level
 * @returns {Set<number>} Set of allowable tool IDs
 */
const allowedTools = (erosionLevel) =>
	erosionLevel % 3 === 0 ? new Set([0, 1]) : // Rocky: Torch (0) or Climbing Gear (1)
		erosionLevel % 3 === 1 ? new Set([1, 2]) : // Wet: Climbing Gear (1) or Neither (2)
			new Set([0, 2]); // Narrow: Torch (0) or Neither (2)

/**
 * Computes the 2D grid of erosion levels for the cave region encompassing the target with margin.
 *
 * @returns {number[][]} 2D array of erosion levels
 */
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

/**
 * Solves Part One: calculates total risk level of the rectangle between (0, 0) and the target.
 *
 * @param {number[][]} grid - Precomputed erosion level grid
 * @returns {number} Sum of risk levels in the target bounding box
 */
function solvePartOne(grid) {
	const [targetX, targetY] = target;
	let totalRisk = 0;

	for (let y = 0; y <= targetY; y++) for (let x = 0; x <= targetX; x++) {
		totalRisk += grid[y][x] % 3;
	}

	return totalRisk;
}

/**
 * Solves Part Two: finds the minimum minutes required to reach the target equipped with the torch (tool 0).
 * Uses Dijkstra's algorithm exploring state transitions of movement (cost 1) and tool swaps (cost 7).
 *
 * @param {number[][]} grid - Precomputed erosion level grid
 * @returns {number} Minimum travel time in minutes
 */
function solvePartTwo(grid) {
	const [targetX, targetY] = target;
	// Priority queue stores [time, x, y, tool] (tool 0 = Torch initially)
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

		// Switch tool in current region (cost +7 minutes)
		const currentRegionType = grid[y][x] % 3;
		for (const newTool of allowedTools(currentRegionType)) {
			if (newTool !== tool) pq.push([time + 7, x, y, newTool]);
		}
	}

	throw new Error('No path found');
}

const grid = calculateRegionGrid();
console.log(`Part One: ${solvePartOne(grid)}`);
console.log(`Part Two: ${solvePartTwo(grid)}`);