/**
 * Day 16: Chronal Classification.
 *
 * Implements a 16-instruction assembly virtual machine operating on 4 registers.
 * Part One parses execution samples (Before state, 4-number instruction, After state), and counts how many behave like 3 or more opcodes.
 * Part Two deduces the 1-to-1 mapping between numeric opcode IDs and their named instruction implementations,
 * then executes the test program to find the value in register 0.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(16)).split('\n').filter(x => x !== '');

/**
 * Map of the 16 virtual machine opcode functions.
 * Each function receives (registers, A, B) and returns the result to be stored in register C.
 */
const opcodes = {
	addr: (regs, A, B) => regs[A] + regs[B],
	addi: (regs, A, B) => regs[A] + B,
	mulr: (regs, A, B) => regs[A] * regs[B],
	muli: (regs, A, B) => regs[A] * B,
	banr: (regs, A, B) => regs[A] & regs[B],
	bani: (regs, A, B) => regs[A] & B,
	borr: (regs, A, B) => regs[A] | regs[B],
	bori: (regs, A, B) => regs[A] | B,
	setr: (regs, A) => regs[A],
	seti: (regs, A) => A,
	gtir: (regs, A, B) => (A > regs[B] ? 1 : 0),
	gtri: (regs, A, B) => (regs[A] > B ? 1 : 0),
	gtrr: (regs, A, B) => (regs[A] > regs[B] ? 1 : 0),
	eqir: (regs, A, B) => (A === regs[B] ? 1 : 0),
	eqri: (regs, A, B) => (regs[A] === B ? 1 : 0),
	eqrr: (regs, A, B) => (regs[A] === regs[B] ? 1 : 0)
};

/**
 * Parses the sample records from the top portion of the puzzle input.
 *
 * @returns {Array<{before: number[], instruction: number[], after: number[]}>} List of parsed samples
 */
function parseInput() {
	const samples = [];
	let i = 0;

	while (i < data.length) {
		if (data[i].startsWith('Before')) {
			const before = JSON.parse(data[i].split(': ')[1]);
			const instruction = data[i + 1].split(' ').map(Number);
			const after = JSON.parse(data[i + 2].split(': ')[1]);
			samples.push({before, instruction, after});
			i += 3;
		} else {
			i++;
		}
	}

	return samples;
}

/**
 * Tests whether executing a given opcode on the 'before' registers with the instruction arguments matches the 'after' state.
 *
 * @param {Function} opcode - The opcode evaluation function
 * @param {number[]} before - Register state before execution
 * @param {number[]} instruction - 4-element instruction [opcodeNumber, A, B, C]
 * @param {number[]} after - Expected register state after execution
 * @returns {boolean} True if the opcode produces the expected after state
 */
function testOpcode(opcode, before, instruction, after) {
	const [_, A, B, C] = instruction;
	const regs = [...before];
	regs[C] = opcode(regs, A, B);
	return JSON.stringify(regs) === JSON.stringify(after);
}

/**
 * Solves Part One: counts how many samples behave like three or more opcodes simultaneously.
 *
 * @returns {number} Count of ambiguous samples with >= 3 matching opcodes
 */
function solvePartOne() {
	const samples = parseInput();
	let count = 0;

	for (const sample of samples) {
		const {before, instruction, after} = sample;
		const matches = Object.values(opcodes).filter(opcode => testOpcode(opcode, before, instruction, after)).length;
		if (matches >= 3) count++;
	}

	return count;
}

/**
 * Solves Part Two: resolves opcode numbers to opcode names via iterative elimination,
 * then executes the test program provided at the end of the input.
 *
 * @returns {number} Value contained in register 0 upon program termination
 */
function solvePartTwo() {
	const samples = parseInput();
	const opcodeOptions = new Map();

	for (const sample of samples) {
		const {before, instruction, after} = sample;
		const opcodeNumber = instruction[0];

		if (!opcodeOptions.has(opcodeNumber)) opcodeOptions.set(opcodeNumber, new Set(Object.keys(opcodes)));

		for (const [fnName, opcodeFn] of Object.entries(opcodes)) {
			if (!testOpcode(opcodeFn, before, instruction, after)) opcodeOptions.get(opcodeNumber).delete(fnName);
		}
	}

	const resolvedOpcodes = {};
	while (Object.keys(resolvedOpcodes).length < Object.keys(opcodes).length) {
		for (const [opcodeNumber, possibleFunctions] of opcodeOptions) {
			if (possibleFunctions.size === 1) {
				const resolvedFn = [...possibleFunctions][0];
				resolvedOpcodes[opcodeNumber] = resolvedFn;
				opcodeOptions.delete(opcodeNumber);

				for (const remainingFunctions of opcodeOptions.values()) remainingFunctions.delete(resolvedFn);
				break;
			}
		}
	}

	const programStartIndex = samples.length * 3;
	const program = data.slice(programStartIndex).map(line => line.split(' ').map(Number));
	const registers = [0, 0, 0, 0];

	for (const [opCodeNumber, A, B, C] of program) {
		const opcodeFnName = resolvedOpcodes[opCodeNumber];
		const opcodeFn = opcodes[opcodeFnName];
		registers[C] = opcodeFn(registers, A, B);
	}

	return registers[0];
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);