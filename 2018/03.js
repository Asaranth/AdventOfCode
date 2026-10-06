/**
 * Day 03: No Matter How You Slice It.
 *
 * Tracks rectangular fabric claims across a 2D coordinate grid.
 * Part One counts how many square inches of fabric are claimed by two or more claims.
 * Part Two identifies the unique claim ID whose rectangle does not overlap with any other claim.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(3)).trim().split('\n');

/**
 * Parses each claim's ID and rectangular region, invoking a callback handler for each covered grid coordinate.
 *
 * @param {function(Map<string, any>, string, number): void} processClaimHandler - Callback receiving the fabric map,
 *                                                                                 coordinate key ("x,y"), and current claim ID.
 * @returns {Map<string, any>} The populated fabric map after visiting all claims
 */
function processClaims(processClaimHandler) {
	const fabric = new Map();

	for (const claim of data) {
		const [id, , at, size] = claim.split(' ');
		const claimId = Number(id.replace('#', ''));
		const [x, y] = at.replace(':', '').split(',').map(Number);
		const [width, height] = size.split('x').map(Number);

		for (let i = x; i < x + width; i++) for (let j = y; j < y + height; j++) {
			const key = `${i},${j}`;
			processClaimHandler(fabric, key, claimId);
		}
	}

	return fabric;
}

/**
 * Counts the number of square inches of fabric claimed by two or more elf claims.
 *
 * @returns {number} The total count of overlapping fabric square inches
 */
function solvePartOne() {
	let overlapCount = 0;

	processClaims((fabric, key) => {
		fabric.set(key, (fabric.get(key) || 0) + 1);
		if (fabric.get(key) === 2) overlapCount++;
	});

	return overlapCount;
}

/**
 * Identifies the single intact claim that has zero overlapping square inches with any other claim.
 *
 * @returns {number|null} The unique non-overlapping claim ID, or null if none found
 */
function solvePartTwo() {
	const overlappingClaims = new Set();
	const allClaims = new Set();

	processClaims((fabric, key, claimId) => {
		allClaims.add(claimId);

		if (!fabric.has(key)) {
			fabric.set(key, [claimId]);
		} else {
			fabric.get(key).forEach(existingClaimId => overlappingClaims.add(existingClaimId));
			overlappingClaims.add(claimId);
			fabric.get(key).push(claimId);
		}
	});

	for (const claimId of allClaims) if (!overlappingClaims.has(claimId)) return claimId;

	return null;
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);