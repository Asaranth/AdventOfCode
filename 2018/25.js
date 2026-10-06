/**
 * Day 25: Four-Dimensional Adventure.
 *
 * Clusters 4-dimensional spacetime points into constellations.
 * Two points belong to the same constellation if the 4D Manhattan distance between them is at most 3,
 * or if there is a chain of connected points linking them (connected components in a graph).
 * Part One / Solution determines the total number of distinct constellations.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(25)).trim().split('\n').map(line => line.split(',').map(Number));

/**
 * Calculates the 4-dimensional Manhattan distance between two spacetime points.
 *
 * @param {number[]} p1 - First 4D point [x, y, z, t]
 * @param {number[]} p2 - Second 4D point [x, y, z, t]
 * @returns {number} The 4D Manhattan distance |dx| + |dy| + |dz| + |dt|
 */
const manhattanDistance = (p1, p2) => Math.abs(p1[0] - p2[0]) + Math.abs(p1[1] - p2[1]) + Math.abs(p1[2] - p2[2]) + Math.abs(p1[3] - p2[3]);

const visited = new Set();
let constellations = 0;

/**
 * Traverses all connected points in the current constellation using Breadth-First Search (BFS).
 *
 * @param {number} startIndex - Index of the seed point for the constellation
 */
function bfs(startIndex) {
	const queue = [startIndex];

	while (queue.length > 0) {
		const current = queue.pop();

		for (let i = 0; i < data.length; i++) {
			if (!visited.has(i) && manhattanDistance(data[current], data[i]) <= 3) {
				visited.add(i);
				queue.push(i);
			}
		}
	}
}

for (let i = 0; i < data.length; i++) {
	if (!visited.has(i)) {
		constellations += 1;
		visited.add(i);
		bfs(i);
	}
}

console.log(`Solution: ${constellations}`);