import {getInputData} from './utils.js';

const [_, players, lastMarble] = (await getInputData(9)).trim().match(/(\d+) players.*?(\d+) points/).map(Number);

class Node {
	constructor(value) {
		this.value = value;
		this.next = null;
		this.prev = null;
	}
}

function simulateGame(multiplier = 1) {
	const scores = Array(players).fill(0);
	const maxMarble = lastMarble * multiplier;
	const root = new Node(0);
	root.next = root;
	root.prev = root;
	let current = root;
	for (let marble = 1; marble <= maxMarble; marble++) {
		if (marble % 23 === 0) {
			const player = (marble - 1) % players;
			scores[player] += marble;
			for (let i = 0; i < 7; i++) current = current.prev;
			scores[player] += current.value;
			current.prev.next = current.next;
			current.next.prev = current.prev;
			current = current.next;
		} else {
			current = current.next;
			const newNode = new Node(marble);
			newNode.next = current.next;
			newNode.prev = current;
			current.next.prev = newNode;
			current.next = newNode;
			current = newNode;
		}
	}
	return Math.max(...scores);
}

console.log(`Part One: ${simulateGame()}`);
console.log(`Part Two: ${simulateGame(100)}`);