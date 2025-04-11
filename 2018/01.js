import {getInputData} from './utils.js';

const data = (await getInputData(1)).split('\n').filter(x => x !== '').map(Number);

function calculateFrequency(stopOnRepeat = false) {
	let frequency = 0;
	const seen = new Set([0]);
	let index = 0;
	while (true) {
		frequency += data[index];
		if (stopOnRepeat && seen.has(frequency)) return frequency;
		seen.add(frequency);
		index = (index + 1) % data.length;
		if (!stopOnRepeat && index === 0) break;
	}
	return frequency;
}

console.log(`Part One: ${calculateFrequency()}`);
console.log(`Part Two: ${calculateFrequency(true)}`);