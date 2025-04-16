import {getInputData} from './utils.js';

const data = (await getInputData(19)).trim().split('\n');
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
	gtir: (reg, a, b, c) => reg[c] = (a > reg[b] ? 1 : 0),
	gtri: (reg, a, b, c) => reg[c] = (reg[a] > b ? 1 : 0),
	gtrr: (reg, a, b, c) => reg[c] = (reg[a] > reg[b] ? 1 : 0),
	eqir: (reg, a, b, c) => reg[c] = (a === reg[b] ? 1 : 0),
	eqri: (reg, a, b, c) => reg[c] = (reg[a] === b ? 1 : 0),
	eqrr: (reg, a, b, c) => reg[c] = (reg[a] === reg[b] ? 1 : 0)
}

function solvePartOne() {
	const instructions = data.slice(1).map(x => {
		const [op, ...args] = x.split(' ');
		return [op, ...args.map(Number)];
	});
	const ipRegister = Number(data[0].split(' ')[1]);
	const registers = Array(6).fill(0);
	while (registers[ipRegister] >= 0 && registers[ipRegister] < instructions.length) {
		const [op, a, b, c] = instructions[registers[ipRegister]];
		operations[op](registers, a, b, c);
		registers[ipRegister]++;
	}
	return registers[0];
}

function solvePartTwo() {
	const a = parseInt(data[22].match(/\d+/g)[1], 10);
	const b = parseInt(data[24].match(/\d+/g)[1], 10);
	let numberToFactorize = 10551236 + a * 22 + b;
	const factors = new Map();
	let possiblePrime = 2;
	while (possiblePrime ** 2 <= numberToFactorize) {
		while (numberToFactorize % possiblePrime === 0) {
			numberToFactorize /= possiblePrime;
			factors.set(possiblePrime, (factors.get(possiblePrime) || 0) + 1);
		}
		possiblePrime++;
	}
	if (numberToFactorize > 1) factors.set(numberToFactorize, 1);
	let sum = 1;
	for (const [prime, power] of factors) sum *= (prime ** (power + 1) - 1) / (prime - 1);
	return sum;
}


console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);