import {getInputData} from './utils.js';

const data = (await getInputData(8)).split(' ').map(Number);

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
		if (numChildren === 0) nodeValue = metadataEntries.reduce((sum, val) => sum + val, 0);
		else nodeValue = metadataEntries.map(idx => childrenValues[idx - 1]).filter(val => val !== undefined).reduce((sum, val) => sum + val, 0);
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