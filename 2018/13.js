/**
 * Day 13: Mine Cart Madness.
 *
 * Simulates mine carts moving along intersecting railroad tracks with turns ('/', '\') and intersections ('+').
 * Cart positions advance tick-by-tick in reading order (top-to-bottom, left-to-right).
 * At intersections, carts cycle their turn decisions: left -> straight -> right -> left.
 * Part One finds the coordinate of the very first cart collision.
 * Part Two removes crashing carts and identifies the final remaining cart's location.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(13)).split('\n').filter(x => x !== '').map(x => x.split(''));

/**
 * Extracts carts from the grid and restores the underlying track symbols beneath them.
 *
 * @returns {{map: string[][], carts: Array<{x: number, y: number, dir: string, nextTurn: string, dead: boolean}>}} Simulation map and cart list
 */
function setupSimulation() {
	const map = data.map(line => line.slice());
	const carts = [];

	for (let y = 0; y < map.length; y++) for (let x = 0; x < map[y].length; x++) {
		const char = map[y][x];
		if ('^v<>'.includes(char)) {
			carts.push({x, y, dir: char, nextTurn: 'left', dead: false});
			map[y][x] = (char === '^' || char === 'v') ? '|' : '-';
		}
	}

	return {map, carts};
}

/**
 * Advances a single cart forward one tile and updates its direction based on track features.
 *
 * @param {{x: number, y: number, dir: string, nextTurn: string}} cart - The cart object
 * @param {string[][]} map - The 2D track layout map
 */
function moveCart(cart, map) {
	const directions = {'^': {x: 0, y: -1}, '>': {x: 1, y: 0}, 'v': {x: 0, y: 1}, '<': {x: -1, y: 0}};
	const turnLeft = {'^': '<', '<': 'v', 'v': '>', '>': '^'};
	const turnRight = {'^': '>', '>': 'v', 'v': '<', '<': '^'};

	const move = directions[cart.dir];
	cart.x += move.x;
	cart.y += move.y;

	const track = map[cart.y][cart.x];

	if (track === '+') {
		if (cart.nextTurn === 'left') {
			cart.dir = turnLeft[cart.dir];
			cart.nextTurn = 'straight';
		} else if (cart.nextTurn === 'straight') {
			cart.nextTurn = 'right';
		} else if (cart.nextTurn === 'right') {
			cart.dir = turnRight[cart.dir];
			cart.nextTurn = 'left';
		}
	} else if (track === '/') {
		if (cart.dir === '^' || cart.dir === 'v') cart.dir = turnRight[cart.dir];
		else cart.dir = turnLeft[cart.dir];
	} else if (track === '\\') {
		if (cart.dir === '^' || cart.dir === 'v') cart.dir = turnLeft[cart.dir];
		else cart.dir = turnRight[cart.dir];
	}
}

/**
 * Solves Part One: simulates mine carts until the first collision occurs.
 *
 * @returns {string} Coordinates of the first collision in "X,Y" format
 */
function solvePartOne() {
	const {map, carts} = setupSimulation();

	while (true) {
		carts.sort((a, b) => a.y === b.y ? a.x - b.x : a.y - b.y);

		for (let i = 0; i < carts.length; i++) {
			const cart = carts[i];
			moveCart(cart, map);

			for (let j = 0; j < carts.length; j++) {
				if (i !== j && carts[i].x === carts[j].x && carts[i].y === carts[j].y) {
					return `${cart.x},${cart.y}`;
				}
			}
		}
	}
}

/**
 * Solves Part Two: removes colliding carts on impact until only one cart remains on the tracks.
 *
 * @returns {string} Coordinates of the last remaining cart in "X,Y" format
 */
function solvePartTwo() {
	let {map, carts} = setupSimulation();

	while (carts.length > 1) {
		carts.sort((a, b) => a.y === b.y ? a.x - b.x : a.y - b.y);

		for (const cart of carts) {
			if (cart.dead) continue;
			moveCart(cart, map);

			const collidingCarts = carts.filter(otherCart => !otherCart.dead && cart.x === otherCart.x && cart.y === otherCart.y);
			if (collidingCarts.length > 1) {
				for (const c of collidingCarts) c.dead = true;
			}
		}

		carts = carts.filter(cart => !cart.dead);
	}

	const lastCart = carts[0];
	return `${lastCart.x},${lastCart.y}`;
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);