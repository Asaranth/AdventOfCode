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

function findFlowEnd(y, x, dx) {
	while (!blocked.has(`${y},${x + dx}`) && blocked.has(`${y + 1},${x}`)) x += dx;
	return x;
}

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