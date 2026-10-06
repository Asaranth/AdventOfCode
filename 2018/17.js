/**
 * Day 17: Reservoir Research.
 *
 * Simulates 2D groundwater flow and accumulation within subterranean clay structures.
 * Water pours from a spring at coordinate (500, 0), falling downwards under gravity until hitting clay or standing water,
 * then spreads laterally. If bounded on both sides, water becomes standing resting water ('~'), otherwise it spills over and flows down.
 * Part One counts all reachable water tiles (both running and standing) within the valid Y-range [minY, maxY].
 * Part Two counts only the retained standing resting water tiles within the same range.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(17)).trim().split('\n');
const clay = 0;
const water = 1;
const blocked = new Map();

for (const line of data) {
	const match = /(\w)=(\d+), (\w)=(\d+)..(\d+)/.exec(line);
	const [, axis, coordinate, _, rangeStart, rangeEnd] = match;
	const isXFixed = axis === 'x';
	const a = parseInt(coordinate, 10);
	const b = parseInt(rangeStart, 10);
	const c = parseInt(rangeEnd, 10);

	for (let i = b; i <= c; i++) {
		const key = isXFixed ? `${i},${a}` : `${a},${i}`;
		blocked.set(key, clay);
	}
}

const minY = Math.min(...[...blocked.keys()].map(key => parseInt(key.split(',')[0], 10)));
const maxY = Math.max(...[...blocked.keys()].map(key => parseInt(key.split(',')[0], 10)));
const visited = new Set();

/**
 * Finds the horizontal stopping point for flowing water in direction dx (left = -1, right = 1).
 * Advances horizontally while the cell below is supported by clay or settled water and the next cell is open.
 *
 * @param {number} y - Current Y coordinate (row)
 * @param {number} x - Starting X coordinate (column)
 * @param {number} dx - Horizontal search direction (-1 for left, 1 for right)
 * @returns {number} The X coordinate where flow stops or spills over
 */
function findFlowEnd(y, x, dx) {
	while (!blocked.has(`${y},${x + dx}`) && blocked.has(`${y + 1},${x}`)) x += dx;
	return x;
}

/**
 * Simulates horizontal spreading of water along a supported surface.
 * If contained on both sides by clay walls, settles the water and recurses upward;
 * otherwise spills downward from open drop-off points.
 *
 * @param {number} y - Y coordinate of the water surface
 * @param {number} x - X coordinate where the falling stream hits the surface
 */
function flowHorizontally(y, x) {
	const left = findFlowEnd(y, x, -1);
	const right = findFlowEnd(y, x, 1);

	for (let i = left; i <= right; i++) visited.add(`${y},${i}`);

	if (!blocked.has(`${y + 1},${left}`) && !visited.has(`${y + 1},${left}`)) flowDown(y, left);
	if (!blocked.has(`${y + 1},${right}`) && !visited.has(`${y + 1},${right}`)) flowDown(y, right);

	if (blocked.has(`${y + 1},${left}`) && blocked.has(`${y + 1},${right}`)) {
		for (let i = left; i <= right; i++) blocked.set(`${y},${i}`, water);
		flowHorizontally(y - 1, x);
	}
}

/**
 * Simulates vertical downward flow of water from a source or spillover.
 *
 * @param {number} y - Starting Y coordinate
 * @param {number} x - X coordinate of the vertical stream
 */
function flowDown(y, x) {
	while (!blocked.has(`${y + 1},${x}`) && y < maxY) {
		y++;
		visited.add(`${y},${x}`);
	}

	if (y < maxY) flowHorizontally(y, x);
}

flowDown(minY - 1, 500);

console.log(`Part One: ${visited.size}`);
console.log(`Part Two: ${[...blocked.values()].filter(x => x === water).length}`);