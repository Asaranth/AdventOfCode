/**
 * Day 21: Chronal Conversion.
 *
 * Analyses an elf assembly activation program to determine initial register 0 values that cause the program to halt.
 * The program only reads register 0 at a single comparison instruction (`eqrr 2 0` or similar), checking if register 2 equals register 0.
 * Part One finds the lowest non-negative integer for register 0 that halts the program in the fewest instructions (the first value compared).
 * Part Two finds the register 0 value that causes the program to execute the most instructions before halting (the last unique value before cycle repetition).
 */

import {getInputData} from './utils.js';

const data = (await getInputData(21)).trim().split('\n');
const instructions = data.slice(1).map(line => {
	const [op, ...args] = line.split(' ');
	return [op, ...args.map(Number)];
});

/**
 * CPU opcode operations modifying registers in place.
 */
const operations = {
	addr: (reg, a, b, c) => reg[c] = reg[a] + reg[b],
	addi: (reg, a, b, c) => reg[c] = reg[a] + b,
	mulr: (reg, a, b, c) => reg[c] = reg[a] * reg[b],
	muli: (reg, a, b, c) => reg[c] = reg[a] * b,
	banr: (reg, a, b, c) => reg[c] = reg[a] & reg[b],
	bani: (reg, a, b, c) => reg[c] = reg[a] & b,
	borr: (reg, a, b, c) => reg[c] = reg[a] | reg[b],
	bori: (reg, a, b, c) => reg[c] = reg[a] | b,
	setr: (reg, a, _, c) => reg[c] = reg[a],
	seti: (reg, a, _, c) => reg[c] = a,
	gtir: (reg, a, b, c) => reg[c] = a > reg[b] ? 1 : 0,
	gtri: (reg, a, b, c) => reg[c] = reg[a] > b ? 1 : 0,
	gtrr: (reg, a, b, c) => reg[c] = reg[a] > reg[b] ? 1 : 0,
	eqir: (reg, a, b, c) => reg[c] = a === reg[b] ? 1 : 0,
	eqri: (reg, a, b, c) => reg[c] = reg[a] === b ? 1 : 0,
	eqrr: (reg, a, b, c) => reg[c] = reg[a] === reg[b] ? 1 : 0
};

/**
 * Solves Part One: intercepts the very first time register 0 is compared against register 2.
 * Setting register 0 to this value causes the program to halt on the first opportunity.
 *
 * @returns {number|null} The lowest register 0 value to halt fastest
 */
function solvePartOne() {
	const ipRegister = Number(data[0].split(' ')[1]);
	let registers = Array(6).fill(0);
	let firstHaltValue = null;

	while (registers[ipRegister] >= 0 && registers[ipRegister] < instructions.length) {
		const [op, a, b, c] = instructions[registers[ipRegister]];
		operations[op](registers, a, b, c);

		if (op === 'eqrr' && a === 2 && b === 0) {
			firstHaltValue = registers[2];
			break;
		}

		registers[ipRegister]++;
	}

	return firstHaltValue;
}

/**
 * Solves Part Two: tracks all values compared against register 0 using a Set until a duplicate is detected.
 * Returns the last unique value before repeating, which yields the maximum instruction count.
 *
 * @returns {number} The register 0 value that executes the longest before halting
 */
function solvePartTwo() {
	const ipRegister = Number(data[0].split(' ')[1]);
	let registers = Array(6).fill(0);
	const seen = new Set();
	let lastHaltValue;

	while (registers[ipRegister] >= 0 && registers[ipRegister] < instructions.length) {
		const [op, a, b, c] = instructions[registers[ipRegister]];
		operations[op](registers, a, b, c);

		if (op === 'eqrr' && a === 2 && b === 0) {
			const haltValue = registers[2];

			if (seen.has(haltValue)) return lastHaltValue;

			seen.add(haltValue);
			lastHaltValue = haltValue;
		}

		registers[ipRegister]++;
	}

	return lastHaltValue;
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);