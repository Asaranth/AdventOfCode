import {getInputData} from './utils.js';

const data = Number(await getInputData(11));
const gridSize = 300;

const generateGrid = () => Array.from({length: gridSize}, (_, y) => Array.from({ length: gridSize }, (_, x) => calculatePowerLevel(x + 1, y + 1)));

function calculatePowerLevel(x, y) {
	const rackID = x + 10;
	let powerLevel = rackID * y;
	powerLevel += data;
	powerLevel *= rackID;
	powerLevel = Math.floor((powerLevel % 1000) / 100);
	return powerLevel - 5;
}

function solvePartOne() {
	const grid = generateGrid();
	let maxPower = Number.MIN_SAFE_INTEGER;
	let topLeftX = 0, topLeftY = 0;
	for (let y = 0; y < gridSize - 2; y++) for (let x = 0; x < gridSize - 2; x++) {
		let currentPower = 0;
		for (let dy = 0; dy < 3; dy++) for (let dx = 0; dx < 3; dx++) currentPower += grid[y + dy][x + dx];
		if (currentPower > maxPower) {
			maxPower = currentPower;
			topLeftX = x + 1;
			topLeftY = y + 1;
		}
	}
	return `${topLeftX},${topLeftY}`;
}

function solvePartTwo() {
	const grid = generateGrid();
	const sat = Array.from({length: gridSize}, () => Array(gridSize).fill(0));
	for (let y = 0; y < gridSize; y++) for (let x = 0; x < gridSize; x++) {
		const cellPower = grid[y][x];
		const top = y > 0 ? sat[y - 1][x] : 0;
		const left = x > 0 ? sat[y][x - 1] : 0;
		const topLeft = x > 0 && y > 0 ? sat[y - 1][x - 1] : 0;
		sat[y][x] = cellPower + top + left - topLeft;
	}
	let maxPower = Number.MIN_SAFE_INTEGER;
	let maxX = 0, maxY = 0, maxSize = 0;
	for (let squareSize = 1; squareSize <= gridSize; squareSize++) for (let y = 0; y < gridSize - squareSize; y++) for (let x = 0; x <= gridSize - squareSize; x++) {
		const bottomRight = sat[y + squareSize - 1][x + squareSize - 1];
		const topRight = y > 0 ? sat[y - 1][x + squareSize - 1] : 0;
		const bottomLeft = x > 0 ? sat[y + squareSize - 1][x - 1] : 0;
		const topLeft = x > 0 && y > 0 ? sat[y - 1][x - 1] : 0;
		const currentPower = bottomRight - topRight - bottomLeft + topLeft;
		if (currentPower > maxPower) {
			maxPower = currentPower;
			maxX = x + 1;
			maxY = y + 1;
			maxSize = squareSize;
		}
	}
	return `${maxX},${maxY},${maxSize}`;
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);