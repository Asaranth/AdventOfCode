import {getInputData} from './utils.js';

const data = (await getInputData(23)).trim().split('\n').map(b => {
	const [_, x, y, z, r] = b.match(/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/).map(Number);
	return {x, y, z, r};
});

function solvePartOne() {
	const strongest = data.reduce((max, bot) => bot.r > max.r ? bot : max, data[0]);
	return data.filter(b => {
		const distance = Math.abs(b.x - strongest.x) + Math.abs(b.y - strongest.y) + Math.abs(b.z - strongest.z);
		return distance <= strongest.r;
	}).length;
}

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
		for (let x = Math.floor(minX / gridSize) * gridSize; x <= maxX; x += gridSize)
			for (let y = Math.floor(minY / gridSize) * gridSize; y <= maxY; y += gridSize)
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