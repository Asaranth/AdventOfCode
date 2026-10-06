import fs from 'fs';
import path from 'path';
import dotenv from 'dotenv';

dotenv.config({path: path.resolve(process.cwd(), '../.env')});

/**
 * Fetches the puzzle input for a specific day from the Advent of Code website,
 * or retrieves it from the local cache if previously downloaded.
 *
 * @param {number} day - The day number of the puzzle (1-25)
 * @returns {Promise<string>} The raw puzzle input text
 * @throws {Error} If AOC_SESSION_COOKIE is missing or if the network request fails
 */
export async function getInputData(day) {
	const cacheFile = `data/${String(day).padStart(2, '0')}.txt`;

	if (fs.existsSync(cacheFile)) return fs.readFileSync(cacheFile, 'utf8');

	const url = `https://adventofcode.com/2018/day/${day}/input`;
	const sessionCookie = process.env.AOC_SESSION_COOKIE;
	if (!sessionCookie) throw new Error('AOC_SESSION_COOKIE not found in environment variables');

	const headers = {Cookie: `session=${sessionCookie}`};

	return fetch(url, {headers}).then(response => {
		if (!response.ok) throw new Error(`Failed to fetch data: ${response.status} - ${response.statusText}`);
		return response.text();
	}).then(data => {
		fs.mkdirSync('data', {recursive: true});
		fs.writeFileSync(cacheFile, data);
		return data;
	}).catch(e => {
		console.error('Error fetching input data:', e.message);
		throw e;
	});
}