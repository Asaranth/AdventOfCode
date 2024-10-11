import os
import re
from datetime import datetime

import requests
from bs4 import BeautifulSoup


def get_aoc_stars(year, user_id, session_cookie):
    session = requests.Session()
    session.cookies.set('session', session_cookie)

    url = f'https://adventofcode.com/{year}/leaderboard/private/view/{user_id}'
    response = session.get(url)
    soup = BeautifulSoup(response.content, 'html.parser')
    star_count = sum(map(int, re.findall(r'class="star-badge">(\d+)</div>', str(soup))))
    return star_count


def main():
    user_id = os.getenv('AOC_USER_ID')
    session_cookie = os.getenv('AOC_SESSION_COOKIE')
    years = range(2015, datetime.now().year + 1)
    stars = {year: get_aoc_stars(year, user_id, session_cookie) for year in years}
    languages = {
        2015: 'R',
        2023: 'Python'
    }

    readme_template = """# Advent of Code 🎄
    
Advent of Code is a delightful online event created by Eric Wastl. It's a coding celebration that happens every December, treating you to daily programming puzzles from the 1st to the 25th. Join the fun at [Advent of Code](https://adventofcode.com/)!
    
## Years and Languages
    
{year_lines}
"""

    year_lines = []
    for year in years:
        if stars[year] == 0:
            year_lines.append(f"- **{year}:** (not started)")
        else:
            language = languages.get(year, "Unknown Language")
            year_lines.append(
                f"- **{year}:** {language} ![Stars](https://img.shields.io/badge/stars-{stars[year]}-yellow)")

    with open('README.md', 'w') as file:
        file.write(readme_template.format(year_lines = "\n".join(year_lines)))


if __name__ == "__main__":
    main()
