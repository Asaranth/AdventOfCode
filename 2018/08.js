/**
 * Day 08: Memory Maneuver.
 *
 * Navigates a tree structure encoded as a flat list of integers.
 * Each node header consists of: [child_count, metadata_count], followed by its child nodes recursively,
 * and finally its metadata entries.
 * Part One calculates the sum of all metadata entries across every node in the tree.
 * Part Two calculates the root node value, where nodes with children use metadata entries as 1-based child indices.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(8)).trim().split(' ').map(Number);

/**
 * Recursively parses a node and its subtrees from the flat list of numbers.
 *
 * @param {number[]} numbers - The full flat array of tree data
 * @param {number} index - The starting index of the current node in the numbers array
 * @param {boolean} [isPartTwo=false] - Whether to evaluate node values using Part Two rules
 * @returns {[number, number]} A tuple of [computed value or metadata sum, next unread index]
 */
function parseNode(numbers, index, isPartTwo = false) {
	const numChildren = numbers[index];
	const numMetadata = numbers[index + 1];
	let currIndex = index + 2;
	const childrenValues = [];
	let metadataSum = 0;

	for (let i = 0; i < numChildren; i++) {
		const [childValue, nextIndex] = parseNode(numbers, currIndex, isPartTwo);
		if (isPartTwo) childrenValues.push(childValue);
		else metadataSum += childValue;
		currIndex = nextIndex;
	}

	const metadataEntries = numbers.slice(currIndex, currIndex + numMetadata);

	if (isPartTwo) {
		let nodeValue;
		if (numChildren === 0) {
			nodeValue = metadataEntries.reduce((sum, val) => sum + val, 0);
		} else {
			nodeValue = metadataEntries
				.map(idx => childrenValues[idx - 1])
				.filter(val => val !== undefined)
				.reduce((sum, val) => sum + val, 0);
		}
		currIndex += numMetadata;
		return [nodeValue, currIndex];
	} else {
		metadataSum += metadataEntries.reduce((sum, val) => sum + val, 0);
		currIndex += numMetadata;
		return [metadataSum, currIndex];
	}
}

console.log(`Part One: ${parseNode(data, 0)[0]}`);
console.log(`Part Two: ${parseNode(data, 0, true)[0]}`);