"""
Common Utilities

Provides HTTP client utilities and puzzle input caching for the 2023 solutions.
"""

import os
import requests
from dotenv import load_dotenv

load_dotenv()


def get_input_data(day):
    """
    Fetches puzzle input for a given day, caching the response locally.

    :param day: Day number of the puzzle (1–25).
    :return: Raw puzzle input string.
    """
    cache_file = f'data/{day:02d}.txt'
    if os.path.exists(cache_file):
        with open(cache_file, 'r') as file:
            return file.read()

    url = f'https://adventofcode.com/2023/day/{day}/input'
    session_cookie = os.getenv('AOC_SESSION_COOKIE')

    if not session_cookie:
        raise ValueError('AOC_SESSION_COOKIE not found in environment variables')

    headers = {
        'Cookie': f'session={session_cookie}'
    }
    response = requests.get(url, headers = headers)

    if response.status_code != 200:
        response.raise_for_status()

    data = response.text

    os.makedirs('data', exist_ok = True)
    with open(cache_file, 'w') as file:
        file.write(data)

    return data