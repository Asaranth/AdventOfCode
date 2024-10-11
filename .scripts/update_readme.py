import argparse
import colorsys
import json
import os
import time
from datetime import datetime

import requests

LANGUAGE_COLORS = {
    'Python': '3572A5',
    'R': '198CE7'
}

SID = os.getenv("AOC_SESSION_COOKIE")
assert SID is not None

UID = os.getenv("AOC_USER_ID")
assert UID is not None

AOC_URL = "https://adventofcode.com/{year}/leaderboard/private/view/{uid}.json"
HEADERS = {
    "User-Agent": "https://github.com/Asaranth/AdventOfCode/blob/main/.scripts/update_readme.py"
}
COOKIES = {"session": SID}
STAR = "⭐"
NUM_YEARS = datetime.now().year - 2015 + 1


def rgb2hex(r, g, b):
    f = lambda x: max(0, min(255, round(x * 255)))
    return f"{f(r):02x}{f(g):02x}{f(b):02x}"


def hsv_interp(t):
    assert 0 <= t <= 1
    return rgb2hex(*colorsys.hsv_to_rgb(h = t * 120 / 360, s = 1, v = 0.6))


def fmt_year_badge(year: int, stars: int, color: str) -> str:
    return f"https://img.shields.io/badge/{year}-{stars}%20{STAR}-{color}?style=flat-square&labelColor=2b2b2b"


def fmt_language_badge(language: str) -> str:
    color = LANGUAGE_COLORS.get(language, 'blue')
    return f"https://img.shields.io/badge/-{language}-{color}?style=flat-square&labelColor=2b2b2b&logo={language}&logoColor=white"


def fmt_total_badge(stars: int, color: str) -> str:
    return f"https://img.shields.io/badge/total-{stars}%20{STAR}-{color}?style=for-the-badge"


def get_year_stars(year: int, sleep_sec: int) -> int:
    res = requests.get(
        AOC_URL.format(year = year, uid = UID),
        headers = HEADERS,
        cookies = COOKIES,
    )
    assert res.status_code == 200
    time.sleep(sleep_sec)
    data = json.loads(res.text)
    return data["members"][UID]["stars"]


def get_year_badge_url(year: int, stars: int) -> str:
    color = hsv_interp(stars / 50)
    return f'<img src="{fmt_year_badge(year, stars, color)}"></img>'


def get_language_badge_url(language: str) -> str:
    return f'<img src="{fmt_language_badge(language)}"></img>'


def get_total_badge_url(stars: int) -> str:
    color = hsv_interp(stars / (NUM_YEARS * 50))
    return f'<a href="./README.md"><img src="{fmt_total_badge(stars, color)}"></img></a>'


def main(args):
    y2s = {y: get_year_stars(y, args.sleep_sec) for y in args.years}
    languages = {
        2015: 'R',
        2023: 'Python'
    }

    if args.total_only:
        print(get_total_badge_url(sum(y2s.values())))
    else:
        for y, s in y2s.items():
            print(f"{get_year_badge_url(y, s)}<br>", end = '')
            if y in languages and s > 0:
                print(f"{get_language_badge_url(languages[y])}<br>", end = '')

    readme_template = """# Advent of Code 🎄

Advent of Code is a delightful online event created by Eric Wastl. It's a coding celebration that happens every December, treating you to daily programming puzzles from the 1st to the 25th. Join the fun at [Advent of Code](https://adventofcode.com/)!

## Years

{year_lines}
"""

    year_lines = []
    for year in args.years:
        line = get_year_badge_url(year, y2s[year])
        if y2s[year] > 0 and year in languages:
            line += f' {get_language_badge_url(languages[year])}'
        year_lines.append(line)

    with open('../README.md', 'w', encoding = 'utf-8') as file:
        file.write(readme_template.format(year_lines = "<br>\n".join(year_lines)))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description = """Generate badge URLs with stars/year.
        The badge color is interpolated with respect to the number of stars: from 0 to 50.""",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--years",
        nargs = "+",
        type = int,
        default = list(range(datetime.now().year, 2014, -1)),
        help = "Years to fetch data from.",
    )
    parser.add_argument(
        "--sleep-sec",
        type = int,
        default = 2,
        help = "Number of seconds to sleep between requests.",
    )
    parser.add_argument(
        "--link-to-dir",
        action = "store_true",
        help = "If given, will link the badge to the corresponding `./<year>` directory.",
    )
    parser.add_argument(
        "--total-only",
        action = "store_true",
        help = "Just generate the total number of stars",
    )
    args = parser.parse_args()
    main(args)
