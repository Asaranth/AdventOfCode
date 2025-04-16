import {getInputData} from './utils.js';

const data = (await getInputData(24)).trim().replace(/points with/g, 'points () with');
const types = ['slashing', 'fire', 'bludgeoning', 'radiation', 'cold'];
const sections = data.split('\n\n');
const immuneInput = sections[0].split('\n').slice(1);
const infectionInput = sections[1].split('\n').slice(1);
const immuneSystem = immuneInput.map(parseGroup);
const infection = infectionInput.map(parseGroup);

const deepCopy = (armies) => JSON.parse(JSON.stringify(armies));

function parseDamage(dmg) {
	const type = dmg.slice(dmg.lastIndexOf(' ') + 1).trim();
	const value = parseInt(dmg.slice(0, dmg.lastIndexOf(' ')));
	return types.map((t) => (t === type ? value : 0));
}

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

function calculateDamage(attacker, defender) {
	const attackPower = Math.max(...attacker.damage);
	const rawDamage = attacker.units * attackPower;
	return rawDamage * defender.resistances[attacker.damage.indexOf(attackPower)];
}

function runCombat(immune, infection) {
	while (immune.length > 0 && infection.length > 0) {
		immune.forEach(group => group.effectivePower = group.units * Math.max(...group.damage));
		infection.forEach(group => group.effectivePower = group.units * Math.max(...group.damage));
		immune.sort((a, b) => b.effectivePower - a.effectivePower || b.initiative - a.initiative);
		infection.sort((a, b) => b.effectivePower - a.effectivePower || b.initiative - a.initiative);
		const immuneTargets = new Map();
		const infectionTargets = new Map();
		immune.forEach(attacker => {
			const target = infection.filter(defender => ![...immuneTargets.values()].includes(defender))
				.map(defender => ({defender, damage: calculateDamage(attacker, defender)}))
				.filter(t => t.damage > 0)
				.sort((a, b) => b.damage - a.damage || b.defender.effectivePower - a.defender.effectivePower || b.defender.initiative - a.defender.initiative)[0];
			if (target) immuneTargets.set(attacker, target.defender);
		});
		infection.forEach(attacker => {
			const target = immune.filter(defender => ![...infectionTargets.values()].includes(defender))
				.map(defender => ({defender, damage: calculateDamage(attacker, defender)}))
				.filter(t => t.damage > 0)
				.sort((a, b) => b.damage - a.damage || b.defender.effectivePower - a.defender.effectivePower || b.defender.initiative - a.defender.initiative)[0];
			if (target) infectionTargets.set(attacker, target.defender);
		});
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
		immune = immune.filter(group => group.units > 0);
		infection = infection.filter(group => group.units > 0);
		if (totalUnitsKilled === 0) return false;
	}
	const immuneCount = immune.reduce((sum, group) => sum + group.units, 0);
	const infectionCount = infection.reduce((sum, group) => sum + group.units, 0);
	return [infectionCount, immuneCount];
}

function boostedCombat(immune, infection, boost) {
	const boostImmune = deepCopy(immune);
	boostImmune.forEach(group => {
		const maxDamageIndex = group.damage.findIndex(dmg => dmg === Math.max(...group.damage));
		group.damage[maxDamageIndex] += boost;
	});
	return runCombat(boostImmune, deepCopy(infection));
}

function solvePartOne() {
	const result = runCombat(deepCopy(immuneSystem), deepCopy(infection));
	return result[0] > 0 ? result[0] : result[1];
}

function solvePartTwo() {
	let low = 1;
	let high = 100;
	while (high > low) {
		const mid = Math.floor((low + high) / 2);
		const result = boostedCombat(immuneSystem, infection, mid);
		if (!result || result[1] === 0) low = mid + 1;
		else high = mid;
	}
	const finalResult = boostedCombat(immuneSystem, infection, high);
	return finalResult[1];
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);