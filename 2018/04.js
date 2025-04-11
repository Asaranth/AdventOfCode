import {getInputData} from './utils.js';

const data = (await getInputData(4)).split('\n').filter((x) => x !== '').map((line) => ({
	date: new Date(line.match(/\[(.*?)]/)[1]),
	line
})).sort((a, b) => a.date - b.date).map((entry) => entry.line);

function parseGuardSleepData() {
	const guardSleepData = {};
	let currentGuard = null;
	let sleepStart = null;
	for (const record of data) {
		const timeMatch = record.match(/\[(\d+-\d+-\d+ \d+:\d+)]/);
		const time = new Date(timeMatch[1]);
		const action = record.slice(19).trim();
		if (action.startsWith('Guard')) {
			const guardMatch = action.match(/Guard #(\d+)/);
			currentGuard = parseInt(guardMatch[1], 10);
		} else if (action === 'falls asleep') {
			sleepStart = time.getMinutes();
		} else if (action === 'wakes up') {
			const sleepEnd = time.getMinutes();
			if (!guardSleepData[currentGuard]) {
				guardSleepData[currentGuard] = {
					totalSleep: 0,
					minutes: Array(60).fill(0)
				};
			}
			guardSleepData[currentGuard].totalSleep += sleepEnd - sleepStart;
			for (let i = sleepStart; i < sleepEnd; i++) guardSleepData[currentGuard].minutes[i]++;
		}
	}
	return guardSleepData;
}

function solvePartOne() {
	let guardSleepData = parseGuardSleepData();
	let sleepiestGuard = null;
	let maxSleep = 0;
	for (const [guard, data] of Object.entries(guardSleepData)) if (data.totalSleep > maxSleep) {
		sleepiestGuard = parseInt(guard, 10);
		maxSleep = data.totalSleep;
	}
	const sleepiestGuardMinutes = guardSleepData[sleepiestGuard].minutes;
	const mostFrequentMinute = sleepiestGuardMinutes.indexOf(Math.max(...sleepiestGuardMinutes));
	return sleepiestGuard * mostFrequentMinute;
}

function solvePartTwo() {
	let guardSleepData = parseGuardSleepData();
	let maxFrequency = 0;
	let sleepiestMinuteGuard = null;
	let sleepiestMinute = null;
	for (const [guard, data] of Object.entries(guardSleepData)) for (let minute = 0; minute < 60; minute++) if (data.minutes[minute] > maxFrequency) {
		maxFrequency = data.minutes[minute];
		sleepiestMinuteGuard = parseInt(guard, 10);
		sleepiestMinute = minute;
	}
	return sleepiestMinuteGuard * sleepiestMinute;
}

console.log(`Part One: ${solvePartOne()}`);
console.log(`Part Two: ${solvePartTwo()}`);