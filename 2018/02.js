/**
 * Day 02: Inventory Management System.
 *
 * Scans warehouse box IDs to verify inventory integrity.
 * Part One computes a checksum by counting box IDs with letters appearing exactly twice and three times, then multiplying the counts.
 * Part Two finds the two box IDs that differ by exactly one character at the same position and returns their common characters.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(2)).trim().split('\n');

/**
 * Computes the checksum of all box IDs by multiplying the count of IDs containing
 * any letter exactly twice by the count of IDs containing any letter exactly three times.
 *
 * @returns {number} The calculated checksum
 */
function solvePartOne() {
	let twoCounts = 0;
	let threeCounts = 0;

	for (const id of data) {
		const charMap = {};
		for (const char of id) charMap[char] = (charMap[char] || 0) + 1;

		if (Object.values(charMap).some(x => x === 2)) twoCounts++;
		if (Object.values(charMap).some(x => x === 3)) threeCounts++;
	}

	return twoCounts * threeCounts;
}

/**
 * Finds the two box IDs that have a Hamming distance of 1 (differing by exactly one character)
 * and returns the string of common characters shared between them.
 *
 * @returns {string|undefined} The common letters between the two correct box IDs
 */
function solvePartTwo() {
	for (let i = 0; i < data.length; i++) for (let j = i + 1; j < data.length; j++) {
		const id1 = data[i];
		const id2 = data[j];
		let differingCount = 0;
		let commonChars = '';

		for (let k = 0; k < id1.length; k++) {
			if (id1[k] === id2[k]) commonChars += id1[k];
			else differingCount++;

			if (differingCount > 1) break;
		}

		if (differingCount === 1) return commonChars;
	}
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);