import {getInputData} from './utils.js';

const data = (await getInputData(21)).trim().split('\n');
const instructions = data.slice(1).map(line => {
	const [op, ...args] = line.split(' ');
	return [op, ...args.map(Number)];
});
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