import {getInputData} from './utils.js';

const data = (await getInputData(15)).trim().split('\n');
const DIRECTIONS = [[0, -1], [-1, 0], [1, 0], [0, 1]];

const inReadingOrder = (a, b) => (a.y === b.y ? a.x - b.x : a.y - b.y);

class Unit {
	constructor(type, x, y, atk) {
		this.type = type;
		this.x = x;
		this.y = y;
		this.hp = 200;
		this.atk = atk;
		this.alive = true;
	}

	isAdjacentTo = (unit) => Math.abs(this.x - unit.x) + Math.abs(this.y - unit.y) === 1;

	attack(enemy) {
		enemy.hp -= this.atk;
		if (enemy.hp <= 0) enemy.alive = false;
	}
}

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
	while (distances.get(`${current.x},${current.y}`).dist > 1) current = distances.get(`${current.x},${current.y}`).parent;
	return {x: current.x, y: current.y};
}

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


function solvePartOne() {
	const {grid, units} = parseInput();
	return simulateCombat(grid, units)[0];
}

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