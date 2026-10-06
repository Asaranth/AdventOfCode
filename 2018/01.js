/**
 * Day 01: Chronal Calibration.
 *
 * Simulates a device's frequency calibration changes from a list of signed integer deltas.
 * Part One sums all frequency changes in a single pass to get the resulting frequency.
 * Part Two continuously cycles through the list to find the first frequency reached twice.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(1)).trim().split('\n').map(Number);

/**
 * Calculates the resulting or first repeated frequency from the input sequence.
 *
 * @param {boolean} [stopOnRepeat=false] - If true, cycles indefinitely until finding the first frequency reached twice (Part Two);
 *                                         if false, returns the final frequency after a single pass through the input (Part One).
 * @returns {number} The resulting or first repeated frequency
 */
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