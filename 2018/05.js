import {getInputData} from './utils.js';

const data = (await getInputData(5)).split('\n')[0];

function reactPolymer(str) {
	const stack = [];
	for (const char of str) {
		const lastChar = stack[stack.length - 1];
		if (lastChar && lastChar.toLowerCase() === char.toLowerCase() && lastChar !== char) stack.pop();
		else stack.push(char);
	}
	return stack.join('')
}

function solvePartTwo() {
	const alphabet = 'abcdefghijklmnopqrstuvwxyz';
	let shortestLength = Infinity;
	for (const char of alphabet) {
		const filteredPolymer = data.replace(new RegExp(char, 'gi'), '');
		const reactedPolymer = reactPolymer(filteredPolymer);
		shortestLength = Math.min(shortestLength, reactedPolymer.length);
	}
	return shortestLength;
}

console.log(`Part One: ${reactPolymer(data).length}`);
console.log(`Part Two: ${solvePartTwo()}`);