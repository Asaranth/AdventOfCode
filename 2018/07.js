import {getInputData} from './utils.js';

const data = (await getInputData(7)).trim().split('\n');

function parseDependencies(data) {
	const dependencyGraph = new Map();
	const regex = /Step (.) must be finished before step (.) can begin\./;
	data.forEach((line) => {
		const [, before, after] = line.match(regex);
		if (!dependencyGraph.has(before)) dependencyGraph.set(before, []);
		if (!dependencyGraph.has(after)) dependencyGraph.set(after, []);
		dependencyGraph.get(after).push(before);
	});
	return dependencyGraph;
}

function solvePartOne() {
	const dependencyGraph = parseDependencies(data);
	const queue = [];
	const result = [];
	for (const [step, dependencies] of dependencyGraph.entries()) if (dependencies.length === 0) queue.push(step);
	queue.sort();
	while (queue.length > 0) {
		const current = queue.shift();
		result.push(current);
		for (const [step, dependencies] of dependencyGraph.entries()) {
			const index = dependencies.indexOf(current);
			if (index !== -1) dependencies.splice(index, 1);
			if (dependencies.length === 0 && !result.includes(step) && !queue.includes(step)) {
				queue.push(step);
				queue.sort();
			}
		}
	}
	return result.join('');
}

function solvePartTwo() {
	const dependencyGraph = parseDependencies(data);
	const inProgress = new Map();
	const completed = new Set();
	const available = [];
	const allSteps = new Set(dependencyGraph.keys());
	let time = 0;
	const workers = Array.from({length: 5}, () => ({step: null, timeLeft: 0}));
	for (const [step, deps] of dependencyGraph.entries()) if (deps.length === 0) available.push(step);
	available.sort();
	while (completed.size < allSteps.size) {
		for (let worker of workers) if (worker.timeLeft === 0 && worker.step) {
			completed.add(worker.step);
			for (const [step, deps] of dependencyGraph.entries()) {
				const index = deps.indexOf(worker.step);
				if (index !== -1) deps.splice(index, 1);
				if (deps.length === 0 && !completed.has(step) && !inProgress.has(step) && !available.includes(step))
					available.push(step);
			}
			inProgress.delete(worker.step);
			worker.step = null;
		}
		available.sort();
		for (let worker of workers) if (worker.timeLeft === 0 && !worker.step && available.length > 0) {
			const nextStep = available.shift();
			worker.step = nextStep;
			worker.timeLeft = 60 + (nextStep.charCodeAt(0) - 'A'.charCodeAt(0) + 1);
			inProgress.set(nextStep, true);
		}
		const allIdle = workers.every(w => w.timeLeft === 0 && w.step === null);
		if (completed.size === allSteps.size && allIdle) break;
		for (let worker of workers) if (worker.timeLeft > 0) worker.timeLeft--;
		time++;
	}
	return time;
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);