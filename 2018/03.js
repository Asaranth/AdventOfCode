import {getInputData} from './utils.js';

const data = (await getInputData(3)).split('\n').filter(x => x !== '');

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

function solvePartOne() {
	let overlapCount = 0;
	processClaims((fabric, key) => {
		fabric.set(key, (fabric.get(key) || 0) + 1);
		if (fabric.get(key) === 2) overlapCount++;
	});
	return overlapCount;
}

function solvePartTwo() {
	const overlappingClaims = new Set();
	const allClaims = new Set();
	processClaims((fabric, key, claimId) => {
		allClaims.add(claimId);
		if (!fabric.has(key)) fabric.set(key, [claimId]);
		else {
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