/**
 * Day 04: Repose Record.
 *
 * Analyses chronological guard shift logs to determine sleep patterns during the midnight hour (00:00 - 00:59).
 * Part One finds the guard with the most total minutes asleep and determines which minute they were asleep most often.
 * Part Two finds the guard who was most frequently asleep on the exact same minute across all recorded days.
 */

import {getInputData} from './utils.js';

const data = (await getInputData(4)).trim().split('\n').map((line) => ({
	date: new Date(line.match(/\[(.*?)]/)[1]),
	line
})).sort((a, b) => a.date - b.date).map((entry) => entry.line);

/**
 * Parses sorted log records and aggregates total sleep duration and minute-by-minute sleep histograms for each guard.
 *
 * @returns {Object<number, {totalSleep: number, minutes: number[]}>} Map of guard IDs to their sleep statistics
 */
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

/**
 * Solves Part One: finds the guard with the highest total minutes of sleep,
 * then multiplies their guard ID by the minute they spent asleep most frequently.
 *
 * @returns {number} Guard ID multiplied by their most frequent sleeping minute
 */
function solvePartOne() {
	const guardSleepData = parseGuardSleepData();
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

/**
 * Solves Part Two: finds the guard most frequently asleep on the same minute,
 * then multiplies their guard ID by that minute.
 *
 * @returns {number} Guard ID multiplied by that specific minute
 */
function solvePartTwo() {
	const guardSleepData = parseGuardSleepData();
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