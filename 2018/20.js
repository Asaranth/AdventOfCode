/**
 * Day 20: A Regular Map.
 *
 * Reconstructs a facility room graph from a regular expression detailing potential navigation paths.
 * Branches ('(...)') and alternatives ('|') represent exploratory branching paths through facility doors.
 * Uses a stack to remember coordinates when entering groups and branching.
 * Part One finds the largest number of doors you must pass through to reach the room furthest from the start.
 * Part Two counts how many rooms have a shortest path distance of at least 1,000 doors from the start.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(20)).trim();

const directions = {N: [0, -1], S: [0, 1], E: [1, 0], W: [-1, 0]};
const map = new Map();

/**
 * Registers an open door (undirected edge) between two adjacent room coordinates.
 *
 * @param {Map<string, Set<string>>} map - The room adjacency graph
 * @param {number} x1 - X coordinate of first room
 * @param {number} y1 - Y coordinate of first room
 * @param {number} x2 - X coordinate of second room
 * @param {number} y2 - Y coordinate of second room
 */
function addDoor(map, x1, y1, x2, y2) {
	if (!map.has(`${x1},${y1}`)) map.set(`${x1},${y1}`, new Set());
	map.get(`${x1},${y1}`).add(`${x2},${y2}`);
}

/**
 * Parses the regular expression route string into a room graph using stack-based branch tracking.
 *
 * @param {string} regex - The regular expression input
 * @returns {[number, number]} Starting coordinate [0, 0]
 */
function parseRegex(regex) {
	let stack = [];
	const start = [0, 0];
	let x = 0, y = 0;

	for (let char of regex) {
		if (char === '^' || char === '$') {
			continue;
		} else if ('NSEW'.includes(char)) {
			const [dx, dy] = directions[char];
			const nx = x + dx;
			const ny = y + dy;
			addDoor(map, x, y, nx, ny);
			addDoor(map, nx, ny, x, y);
			x = nx;
			y = ny;
		} else if (char === '(') {
			stack.push([x, y]);
		} else if (char === '|') {
			[x, y] = stack[stack.length - 1];
		} else if (char === ')') {
			[x, y] = stack.pop();
		}
	}

	return start;
}

/**
 * Computes shortest path door distances from the starting room to all reachable rooms using Breadth-First Search.
 *
 * @param {[number, number]} start - Starting coordinate pair [0, 0]
 * @returns {Map<string, number>} Map of room coordinate string keys to shortest door distances
 */
function bfs(start) {
	const queue = [start];
	const distances = new Map();
	distances.set(`${start[0]},${start[1]}`, 0);

	while (queue.length > 0) {
		const [x, y] = queue.shift();
		const distance = distances.get(`${x},${y}`);

		for (let neighbor of map.get(`${x},${y}`) || []) {
			if (!distances.has(neighbor)) {
				const [nx, ny] = neighbor.split(',').map(Number);
				distances.set(neighbor, distance + 1);
				queue.push([nx, ny]);
			}
		}
	}

	return distances;
}

const start = parseRegex(data);
const distances = bfs(start);

console.log(`Part One: ${Math.max(...distances.values())}`);
console.log(`Part Two: ${[...distances.values()].filter(distance => distance >= 1000).length}`);