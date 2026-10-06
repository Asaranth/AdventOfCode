/**
 * Day 15: Beverage Bandits.
 *
 * Simulates a grid-based tactical combat game between Elves ('E') and Goblins ('G').
 * Units take turns in reading order (top-to-bottom, left-to-right).
 * During a turn, a unit moves toward the nearest open space adjacent to an enemy using BFS pathfinding (tie-broken by reading order),
 * then attacks an adjacent enemy with the lowest hit points (tie-broken by reading order).
 * Part One simulates the combat with default attack power (3) and computes outcome = full_rounds * total_remaining_hp.
 * Part Two finds the minimum Elf attack power such that all Elves survive and win the battle.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(15)).trim().split('\n');

const DIRECTIONS = [[0, -1], [-1, 0], [1, 0], [0, 1]];

/**
 * Comparator for sorting positions or units in reading order (top-to-bottom, left-to-right).
 *
 * @param {{x: number, y: number}} a - First object with x, y coordinates
 * @param {{x: number, y: number}} b - Second object with x, y coordinates
 * @returns {number} Negative if a comes before b, positive if after
 */
const inReadingOrder = (a, b) => (a.y === b.y ? a.x - b.x : a.y - b.y);

/**
 * Represents an active combatant (Elf or Goblin) on the grid.
 */
class Unit {
	/**
	 * Creates a new combat unit.
	 *
	 * @param {'E'|'G'} type - Unit faction ('E' for Elf, 'G' for Goblin)
	 * @param {number} x - Current X coordinate
	 * @param {number} y - Current Y coordinate
	 * @param {number} atk - Attack damage power
	 */
	constructor(type, x, y, atk) {
		this.type = type;
		this.x = x;
		this.y = y;
		this.hp = 200;
		this.atk = atk;
		this.alive = true;
	}

	/**
	 * Checks whether this unit is orthogonally adjacent to another unit.
	 *
	 * @param {Unit} unit - Other unit to compare
	 * @returns {boolean} True if adjacent, false otherwise
	 */
	isAdjacentTo = (unit) => Math.abs(this.x - unit.x) + Math.abs(this.y - unit.y) === 1;

	/**
	 * Attacks an enemy unit, reducing their hit points and marking them dead if HP drops to 0 or below.
	 *
	 * @param {Unit} enemy - The target enemy unit
	 */
	attack(enemy) {
		enemy.hp -= this.atk;
		if (enemy.hp <= 0) enemy.alive = false;
	}
}

/**
 * Parses the puzzle input into an empty cave grid and a list of active combat units.
 *
 * @param {number} [elfAtk=3] - Attack power assigned to Elf units
 * @returns {{grid: string[][], units: Unit[]}} The cave grid map and units array
 */
function parseInput(elfAtk = 3) {
	const grid = [];
	const units = [];

	data.forEach((line, y) => {
		grid.push(line.split(''));
		line.split('').forEach((char, x) => {
			if (char === 'E' || char === 'G') {
				units.push(new Unit(char, x, y, char === 'E' ? elfAtk : 3));
				grid[y][x] = '.';
			}
		});
	});

	return {grid, units};
}

/**
 * Computes shortest distances from a starting position to all reachable grid cells using BFS.
 *
 * @param {string[][]} grid - Cave layout map
 * @param {Unit[]} units - All active units (used for obstacle checking)
 * @param {{x: number, y: number}} startPosition - Starting grid coordinates
 * @returns {Map<string, {dist: number, parent: {x: number, y: number}}>} Map of coordinate keys to the shortest distance and parent coordinate
 */
function computeDistances(grid, units, startPosition) {
	const queue = [[startPosition.x, startPosition.y, 0]];
	const visited = new Set([`${startPosition.x},${startPosition.y}`]);
	const distances = new Map();

	while (queue.length > 0) {
		const [x, y, dist] = queue.shift();

		for (const [dx, dy] of DIRECTIONS) {
			const nx = x + dx;
			const ny = y + dy;
			const key = `${nx},${ny}`;

			if (grid[ny]?.[nx] === '.' && !units.some((u) => u.alive && u.x === nx && u.y === ny) && !visited.has(key)) {
				visited.add(key);
				distances.set(key, {dist: dist + 1, parent: {x, y}});
				queue.push([nx, ny, dist + 1]);
			}
		}
	}

	return distances;
}

/**
 * Identifies the best initial step for a unit toward the nearest reachable target destination in reading order.
 *
 * @param {Unit} unit - The unit attempting to move
 * @param {Map<string, {dist: number, parent: {x: number, y: number}}>} distances - BFS distance map from current position
 * @param {Array<{x: number, y: number}>} targets - Potential destination squares adjacent to targets
 * @returns {{x: number, y: number}|null} The next step coordinates or null if no path exists
 */
function findBestMove(unit, distances, targets) {
	let closest = null;
	let bestDist = Infinity;
	const sortedTargets = targets.sort((a, b) => (a.y === b.y ? a.x - b.x : a.y - b.y));

	for (const target of sortedTargets) {
		const targetKey = `${target.x},${target.y}`;
		const data = distances.get(targetKey);

		if (data && data.dist < bestDist) {
			bestDist = data.dist;
			closest = target;
		} else if (data && data.dist === bestDist) {
			if (closest && (target.y < closest.y || (target.y === closest.y && target.x < closest.x))) closest = target;
		}
	}

	if (!closest) return null;

	let current = closest;
	while (distances.get(`${current.x},${current.y}`).dist > 1) {
		current = distances.get(`${current.x},${current.y}`).parent;
	}

	return {x: current.x, y: current.y};
}

/**
 * Simulates a single complete round of combat across all living units in reading order.
 *
 * @param {string[][]} grid - Cave map
 * @param {Unit[]} units - Active combatants
 * @returns {boolean} True if the round finished with active combat, false if combat terminated early
 */
function simulateRound(grid, units) {
	units.sort(inReadingOrder);

	for (const unit of units) {
		if (!unit.alive) continue;

		const enemies = units.filter((u) => u.alive && u.type !== unit.type);
		if (enemies.length === 0) return false;

		const targetPositions = new Set(enemies.flatMap((enemy) => DIRECTIONS.map(([dx, dy]) => ({
			x: enemy.x + dx,
			y: enemy.y + dy
		}))).filter(({x, y}) => grid[y]?.[x] === '.' && !units.some((u) => u.alive && u.x === x && u.y === y)));

		const adjacentEnemies = enemies.filter((enemy) => unit.isAdjacentTo(enemy));

		if (adjacentEnemies.length > 0) {
			const target = adjacentEnemies.sort((a, b) => a.hp - b.hp || inReadingOrder(a, b))[0];
			unit.attack(target);
			continue;
		}

		if (targetPositions.size > 0) {
			const distances = computeDistances(grid, units, unit);
			const move = findBestMove(unit, distances, Array.from(targetPositions));
			if (move) {
				unit.x = move.x;
				unit.y = move.y;
			}
		}

		const enemiesAfterMove = enemies.filter((enemy) => unit.isAdjacentTo(enemy));
		if (enemiesAfterMove.length > 0) {
			const target = enemiesAfterMove.sort((a, b) => a.hp - b.hp || inReadingOrder(a, b))[0];
			unit.attack(target);
		}
	}

	return true;
}

/**
 * Simulates combat until one faction is eliminated.
 *
 * @param {string[][]} grid - Cave layout map
 * @param {Unit[]} units - Combatants array
 * @returns {[number, boolean]} A tuple of [outcome score (rounds * remaining HP), true]
 */
function simulateCombat(grid, units) {
	let rounds = 0;

	while (true) {
		const combatContinues = simulateRound(grid, units);
		if (!combatContinues) break;
		rounds++;
	}

	const remainingHp = units.filter((u) => u.alive).reduce((sum, u) => sum + u.hp, 0);
	return [rounds * remainingHp, true];
}

/**
 * Solves Part One: runs the battle with standard Elf attack power 3.
 *
 * @returns {number} The battle outcome score
 */
function solvePartOne() {
	const {grid, units} = parseInput();
	return simulateCombat(grid, units)[0];
}

/**
 * Solves Part Two: tests increasing Elf attack powers until Elves win with 0 casualties.
 *
 * @returns {number} The outcome score of the flawless Elf victory
 */
function solvePartTwo() {
	let elfAtk = 4;

	while (true) {
		const {grid, units} = parseInput(elfAtk);
		const [result, elvesWin] = simulateCombat(grid, units);

		if (elvesWin && units.filter((u) => u.type === 'E').every((e) => e.alive)) return result;
		elfAtk++;
	}
}

console.log('Part One:', solvePartOne());
console.log('Part Two:', solvePartTwo());