/**
 * Day 23: Experimental Emergency Teleportation.
 *
 * Analyses a 3D coordinate space populated with nanobots, each having a 3D position (x, y, z) and a signal radius r.
 * Distance is measured via 3D Manhattan distance (|dx| + |dy| + |dz|).
 * Part One finds the nanobot with the largest signal radius and counts how many nanobots are in its range.
 * Part Two finds the 3D integer coordinate in range of the maximum number of nanobots (tie-broken by distance to origin (0,0,0))
 * using a hierarchical octree / multi-scale grid subdivision approach.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(23)).trim().split('\n').map(b => {
	const [_, x, y, z, r] = b.match(/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/).map(Number);
	return {x, y, z, r};
});

/**
 * Solves Part One: finds the nanobot with the largest radius and counts all nanobots within its range.
 *
 * @returns {number} Count of nanobots in range of the strongest bot
 */
function solvePartOne() {
	const strongest = data.reduce((max, bot) => bot.r > max.r ? bot : max, data[0]);

	return data.filter(b => {
		const distance = Math.abs(b.x - strongest.x) + Math.abs(b.y - strongest.y) + Math.abs(b.z - strongest.z);
		return distance <= strongest.r;
	}).length;
}

/**
 * Solves Part Two: locates the coordinate in range of the maximum number of nanobots using hierarchical 3D search.
 * Progressively halves the search resolution (gridSize) around the best coordinate until reaching a resolution of 1 unit.
 *
 * @returns {number} Manhattan distance from origin (0, 0, 0) to the optimal coordinate
 */
function solvePartTwo() {
	let minX = Infinity, minY = Infinity, minZ = Infinity;
	let maxX = -Infinity, maxY = -Infinity, maxZ = -Infinity;

	for (const bot of data) {
		minX = Math.min(minX, bot.x - bot.r);
		maxX = Math.max(maxX, bot.x + bot.r);
		minY = Math.min(minY, bot.y - bot.r);
		maxY = Math.max(maxY, bot.y + bot.r);
		minZ = Math.min(minZ, bot.z - bot.r);
		maxZ = Math.max(maxZ, bot.z + bot.r);
	}

	let gridSize = 1;
	while (gridSize < Math.max(maxX - minX, maxY - minY, maxZ - minZ)) gridSize *= 2;

	let bestCoordinate = null;
	let bestCount = 0;
	let bestDistance = Infinity;

	while (gridSize >= 1) {
		for (let x = Math.floor(minX / gridSize) * gridSize; x <= maxX; x += gridSize) {
			for (let y = Math.floor(minY / gridSize) * gridSize; y <= maxY; y += gridSize) {
				for (let z = Math.floor(minZ / gridSize) * gridSize; z <= maxZ; z += gridSize) {
					let count = 0;

					for (const bot of data) {
						const distance = Math.abs(bot.x - x) + Math.abs(bot.y - y) + Math.abs(bot.z - z);
						if (distance <= bot.r) count++;
					}

					const distanceToOrigin = Math.abs(x) + Math.abs(y) + Math.abs(z);

					if (count > bestCount || (count === bestCount && distanceToOrigin < bestDistance)) {
						bestCount = count;
						bestDistance = distanceToOrigin;
						bestCoordinate = {x, y, z};
					}
				}
			}
		}

		minX = bestCoordinate.x - gridSize;
		maxX = bestCoordinate.x + gridSize;
		minY = bestCoordinate.y - gridSize;
		maxY = bestCoordinate.y + gridSize;
		minZ = bestCoordinate.z - gridSize;
		maxZ = bestCoordinate.z + gridSize;
		gridSize = Math.floor(gridSize / 2);
	}

	return bestDistance;
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);