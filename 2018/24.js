/**
 * Day 24: Immune System Simulator 20XX.
 *
 * Simulates a battle between two armies: the Immune System and the Infection.
 * Combat proceeds in rounds consisting of two phases:
 * 1. Target Selection: groups choose targets in decreasing order of effective power (units * damage) and initiative.
 *    A target is chosen based on maximum potential damage dealt, tie-broken by effective power and initiative.
 * 2. Attacking: groups attack their chosen targets in decreasing order of initiative.
 * Part One simulates standard combat and returns the number of surviving units in the winning army.
 * Part Two finds the minimum integer attack damage boost for the Immune System to win the simulation (using binary search).
 */

import {getInputData} from './utils.js';

// Parse raw army configurations
const data = (await getInputData(24)).trim().replace(/points with/g, 'points () with');
const types = ['slashing', 'fire', 'bludgeoning', 'radiation', 'cold'];
const sections = data.split('\n\n');
const immuneInput = sections[0].split('\n').slice(1);
const infectionInput = sections[1].split('\n').slice(1);
const immuneSystem = immuneInput.map(parseGroup);
const infection = infectionInput.map(parseGroup);

/**
 * Creates a deep clone of army group objects.
 *
 * @param {Array<Object>} armies - Array of army unit groups
 * @returns {Array<Object>} Deep copy of the array
 */
const deepCopy = (armies) => JSON.parse(JSON.stringify(armies));

/**
 * Parses damage string descriptor into a damage array aligned with `types`.
 *
 * @param {string} dmg - Damage descriptor (e.g. "25 cold")
 * @returns {number[]} Array of damage values per damage type
 */
function parseDamage(dmg) {
	const type = dmg.slice(dmg.lastIndexOf(' ') + 1).trim();
	const value = parseInt(dmg.slice(0, dmg.lastIndexOf(' ')));
	return types.map((t) => (t === type ? value : 0));
}

/**
 * Parses weakness/immunity parenthetical clauses into a multiplier array aligned with `types`.
 *
 * @param {string} res - Semicolon-separated resistance clause (e.g. "weak to fire; immune to cold")
 * @returns {number[]} Multipliers per type: 2 (weak), 0 (immune), 1 (normal)
 */
function parseResistance(res) {
	const mult = [1, 1, 1, 1, 1];
	if (!res) return mult;

	res.split('; ').forEach(r => {
		const factor = r.startsWith('weak') ? 2 : r.startsWith('immune') ? 0 : 1;
		const start = r.indexOf('to') + 3;
		r.slice(start).split('&').forEach(t => mult[types.indexOf(t)] = factor);
	});

	return mult;
}

/**
 * Parses a single text line into a structured unit group object.
 *
 * @param {string} line - Raw input description of the unit group
 * @returns {Object} Structured group attributes
 */
function parseGroup(line) {
	const [units, hp, res, dmg, initiative] = line.replace(/, /g, '&')
		.replace(' units each with ', ',')
		.replace(' hit points (', ',')
		.replace(') with an attack that does ', ',')
		.replace(' damage at initiative ', ',')
		.split(',');

	return {
		units: parseInt(units),
		hp: parseInt(hp),
		resistances: parseResistance(res),
		damage: parseDamage(dmg),
		initiative: parseInt(initiative),
		effectivePower: 0
	};
}

/**
 * Calculates the exact damage an attacking group would deal to a defending group.
 *
 * @param {Object} attacker - The attacking group
 * @param {Object} defender - The defending group
 * @returns {number} Computed damage considering weaknesses/immunities
 */
function calculateDamage(attacker, defender) {
	const attackPower = Math.max(...attacker.damage);
	const rawDamage = attacker.units * attackPower;
	return rawDamage * defender.resistances[attacker.damage.indexOf(attackPower)];
}

/**
 * Simulates a full combat engagement between the Immune System and the Infection.
 *
 * @param {Array<Object>} immune - Immune system groups
 * @param {Array<Object>} infection - Infection groups
 * @returns {[number, number]|false} [infectionRemainingUnits, immuneRemainingUnits] or false if stalemate occurs
 */
function runCombat(immune, infection) {
	while (immune.length > 0 && infection.length > 0) {
		// Calculate effective power for each group
		immune.forEach(group => group.effectivePower = group.units * Math.max(...group.damage));
		infection.forEach(group => group.effectivePower = group.units * Math.max(...group.damage));

		// Sort groups for target selection: descending effective power, tie-broken by initiative
		immune.sort((a, b) => b.effectivePower - a.effectivePower || b.initiative - a.initiative);
		infection.sort((a, b) => b.effectivePower - a.effectivePower || b.initiative - a.initiative);

		const immuneTargets = new Map();
		const infectionTargets = new Map();

		// Phase 1: Target Selection for Immune System
		immune.forEach(attacker => {
			const target = infection.filter(defender => ![...immuneTargets.values()].includes(defender))
				.map(defender => ({defender, damage: calculateDamage(attacker, defender)}))
				.filter(t => t.damage > 0)
				.sort((a, b) => b.damage - a.damage || b.defender.effectivePower - a.defender.effectivePower || b.defender.initiative - a.defender.initiative)[0];
			if (target) immuneTargets.set(attacker, target.defender);
		});

		// Phase 1: Target Selection for Infection
		infection.forEach(attacker => {
			const target = immune.filter(defender => ![...infectionTargets.values()].includes(defender))
				.map(defender => ({defender, damage: calculateDamage(attacker, defender)}))
				.filter(t => t.damage > 0)
				.sort((a, b) => b.damage - a.damage || b.defender.effectivePower - a.defender.effectivePower || b.defender.initiative - a.defender.initiative)[0];
			if (target) infectionTargets.set(attacker, target.defender);
		});

		// Phase 2: Attacking in descending order of initiative
		const allGroups = [...immune.map(g => ({group: g, side: 'immune'})), ...infection.map(g => ({
			group: g,
			side: 'infection'
		}))];
		allGroups.sort((a, b) => b.group.initiative - a.group.initiative);

		let totalUnitsKilled = 0;

		for (const {group: attacker, side} of allGroups) {
			if (attacker.units <= 0) continue;
			const targets = side === 'immune' ? immuneTargets : infectionTargets;
			const defender = targets.get(attacker);
			if (!defender) continue;

			const damage = calculateDamage(attacker, defender);
			const unitsKilled = Math.min(defender.units, Math.floor(damage / defender.hp));
			defender.units -= unitsKilled;
			totalUnitsKilled += unitsKilled;
		}

		// Prune dead groups
		immune = immune.filter(group => group.units > 0);
		infection = infection.filter(group => group.units > 0);

		// Stalemate detection (no units killed during round)
		if (totalUnitsKilled === 0) return false;
	}

	const immuneCount = immune.reduce((sum, group) => sum + group.units, 0);
	const infectionCount = infection.reduce((sum, group) => sum + group.units, 0);
	return [infectionCount, immuneCount];
}

/**
 * Runs a battle where the Immune System receives an attack power boost.
 *
 * @param {Array<Object>} immune - Immune system groups
 * @param {Array<Object>} infection - Infection groups
 * @param {number} boost - Extra damage points added to each immune group
 * @returns {[number, number]|false} Result of runCombat with boosted values
 */
function boostedCombat(immune, infection, boost) {
	const boostImmune = deepCopy(immune);
	boostImmune.forEach(group => {
		const maxDamageIndex = group.damage.findIndex(dmg => dmg === Math.max(...group.damage));
		group.damage[maxDamageIndex] += boost;
	});
	return runCombat(boostImmune, deepCopy(infection));
}

/**
 * Solves Part One: runs the battle without any attack boost.
 *
 * @returns {number} Total surviving units of the winning side
 */
function solvePartOne() {
	const result = runCombat(deepCopy(immuneSystem), deepCopy(infection));
	return result[0] > 0 ? result[0] : result[1];
}

/**
 * Solves Part Two: performs binary search to find the minimal boost for the Immune System to win.
 *
 * @returns {number} Number of surviving Immune System units with the minimal winning boost
 */
function solvePartTwo() {
	let low = 1;
	let high = 100;

	while (high > low) {
		const mid = Math.floor((low + high) / 2);
		const result = boostedCombat(immuneSystem, infection, mid);

		if (!result || result[1] === 0) {
			low = mid + 1; // Infection won or tied; need a higher boost
		} else {
			high = mid; // Immune system won; try to find a smaller boost
		}
	}

	const finalResult = boostedCombat(immuneSystem, infection, high);
	return finalResult[1];
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);