import {getInputData} from './utils.js';

const data = Number(await getInputData(14));

function generateRecipes(scoreboard, elf1, elf2) {
	const newRecipeSum = scoreboard[elf1] + scoreboard[elf2];
	const newRecipes = newRecipeSum.toString().split('').map(Number);
	scoreboard.push(...newRecipes);
	elf1 = (elf1 + 1 + scoreboard[elf1]) % scoreboard.length;
	elf2 = (elf2 + 1 + scoreboard[elf2]) % scoreboard.length;
	return {scoreboard, elf1, elf2};
}

function checkTargetSequence(scoreboard, targetSequence) {
	const targetLength = targetSequence.length;
	if (scoreboard.slice(-targetLength).join('') === targetSequence) return scoreboard.length - targetLength;
	if (scoreboard.slice(-(targetLength + 1), -1).join('') === targetSequence) return scoreboard.length - targetLength - 1;
	return null;
}

function solvePartOne() {
	let scoreboard = [3, 7], elf1 = 0, elf2 = 1;
	while (scoreboard.length < data + 10) ({scoreboard, elf1, elf2} = generateRecipes(scoreboard, elf1, elf2));
	return scoreboard.slice(data, data + 10).join('');
}

function solvePartTwo() {
	let scoreboard = [3, 7], elf1 = 0, elf2 = 1;
	const targetSequence = data.toString();
	while (true) {
		({scoreboard, elf1, elf2} = generateRecipes(scoreboard, elf1, elf2));
		const sequenceMatchIndex = checkTargetSequence(scoreboard, targetSequence);
		if (sequenceMatchIndex !== null) return sequenceMatchIndex;
	}
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);