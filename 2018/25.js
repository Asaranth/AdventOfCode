import {getInputData} from './utils.js';

const data = (await getInputData(25)).trim().split('\n').map(line => line.split(',').map(Number));

const manhattanDistance = (p1, p2) => Math.abs(p1[0] - p2[0]) + Math.abs(p1[1] - p2[1]) + Math.abs(p1[2] - p2[2]) + Math.abs(p1[3] - p2[3]);

const visited = new Set();
let constellations = 0;

function bfs(startIndex) {
	const queue = [startIndex];
	while (queue.length > 0) {
		const current = queue.pop();
		for (let i = 0; i < data.length; i++) if (!visited.has(i) && manhattanDistance(data[current], data[i]) <= 3) {
			visited.add(i);
			queue.push(i);
		}
	}
}

for (let i = 0; i < data.length; i++) if (!visited.has(i)) {
	constellations += 1;
	visited.add(i);
	bfs(i);
}

console.log(`Solution: ${constellations}`);