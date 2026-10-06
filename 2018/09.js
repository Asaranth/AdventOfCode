/**
 * Day 09: Marble Mania.
 *
 * Simulates a circular marble placement game played by elves using a circular doubly-linked list.
 * Normal turns place a marble between 1 and 2 positions clockwise.
 * Multiples of 23 award points: the current player scores the marble plus the marble 7 positions counter-clockwise.
 * Part One runs the simulation for the given number of marbles.
 * Part Two runs the simulation for 100 times as many marbles.
 */

import {getInputData} from './utils.js';

const [_, players, lastMarble] = (await getInputData(9)).trim().match(/(\d+) players.*?(\d+) points/).map(Number);

/**
 * Node in a doubly-linked circular list representing a single marble.
 */
class Node {
	/**
	 * Creates a new node with the specified marble value.
	 *
	 * @param {number} value - The numeric value of the marble
	 */
	constructor(value) {
		this.value = value;
		this.next = null;
		this.prev = null;
	}
}

/**
 * Simulates the marble game using a circular doubly linked list and returns the highest player score.
 *
 * @param {number} [multiplier=1] - Multiplier applied to the last marble value (1 for Part One, 100 for Part Two)
 * @returns {number} The maximum score achieved by any player
 */
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