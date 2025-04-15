import {getInputData} from './utils.js';

const data = (await getInputData(10)).trim().split('\n').map(x => {
	const match = x.match(/position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/);
	if (match) return { x: parseInt(match[1]), y: parseInt(match[2]), vx: parseInt(match[3]), vy: parseInt(match[4]) };
});

function solve() {
	let time = 0;
	let smallestBoundingBox = Infinity;
	let alignedPoints;
	while (true) {
		data.forEach(point => { point.x += point.vx; point.y += point.vy; });
		const minX = Math.min(...data.map(p => p.x));
		const maxX = Math.max(...data.map(p => p.x));
		const minY = Math.min(...data.map(p => p.y));
		const maxY = Math.max(...data.map(p => p.y));
		const area = (maxX - minX) * (maxY - minY);
		if (area < smallestBoundingBox) {
			smallestBoundingBox = area;
			alignedPoints = JSON.parse(JSON.stringify(data));
			time++;
		} else break;
	}
	const minX = Math.min(...alignedPoints.map(p => p.x));
	const maxX = Math.max(...alignedPoints.map(p => p.x));
	const minY = Math.min(...alignedPoints.map(p => p.y));
	const maxY = Math.max(...alignedPoints.map(p => p.y));
	const result = Array(maxY - minY + 1).fill(undefined).map(_ => Array(maxX - minX + 1).fill(' '));
	alignedPoints.forEach(point => result[point.y - minY][point.x - minX] = '#');
	return [`\n${result.map(row => row.join('')).join('\n')}`, time];
}

let [result, time] = solve();
console.log(`Part One: ${result}`);
console.log(`Part Two: ${time}`);