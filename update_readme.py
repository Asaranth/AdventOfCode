#!/usr/bin/env python3

import argparse
import colorsys
import json
import os
import time
from datetime import datetime

import requests


def rgb2hex(r, g, b):
    f = lambda x: max(0, min(255, round(x * 255)))
    return f"{f(r):02x}{f(g):02x}{f(b):02x}"


def hsv_interp(t):
    # 0 - 60 - 120
    assert 0 <= t <= 1
    return rgb2hex(*colorsys.hsv_to_rgb(h = t * 120 / 360, s = 1, v = 0.6))


# cookie session from .env
SID = os.getenv("AOC_SESSION_COOKIE")
assert SID is not None

# personal ID from .env
UID = os.getenv("AOC_USER_ID")
assert UID is not None

AOC_URL = "https://adventofcode.com/{year}/leaderboard/private/view/{uid}.json"
HEADERS = {
    "User-Agent": "https://github.com/alexandru-dinu/advent-of-code/blob/main/.scripts/gen_badges.py"
}
COOKIES = {"session": SID}
STAR = "⭐"
NUM_YEARS = datetime.now().year - 2015 + 1


def fmt_year_badge(year: int, stars: int, color: str) -> str:
    return f"https://img.shields.io/badge/{year}-{stars}%20{STAR}-{color}?style=flat-square"


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


def get_year_badge_url(year: int, stars: int, link_to_dir: bool) -> str:
    color = hsv_interp(stars / 50)

    badge = f'<img src="{fmt_year_badge(year, stars, color)}"></img>'
    if link_to_dir:
        badge = f'<a href="./{year}">{badge}</a>'

    return badge


def get_total_badge_url(stars: int) -> str:
    color = hsv_interp(stars / (NUM_YEARS * 50))

    return (
        f'<a href="./README.md"><img src="{fmt_total_badge(stars, color)}"></img></a>'
    )


def main(args):
    y2s = {y: get_year_stars(y, args.sleep_sec) for y in args.years}

    if args.total_only:
        print(get_total_badge_url(sum(y2s.values())))
    else:
        for y, s in y2s.items():
            print(get_year_badge_url(y, s, args.link_to_dir))

    # Generate README.md content
    readme_template = """# Advent of Code 🎄

Advent of Code is a delightful online event created by Eric Wastl. It's a coding celebration that happens every December, treating you to daily programming puzzles from the 1st to the 25th. Join the fun at [Advent of Code](https://adventofcode.com/)!

## Years and Languages

{year_lines}
"""
    languages = {
        2015: 'R',
        2023: 'Python'
    }

    year_lines = []
    for year in args.years:
        if y2s[year] == 0:
            year_lines.append(f"- **{year}:** (not started)")
        else:
            language = languages.get(year, "Unknown Language")
            year_lines.append(
                f"- **{year}:** {language} ![Stars](https://img.shields.io/badge/stars-{y2s[year]}-yellow)")

    with open('README.md', 'w', encoding = 'utf-8') as file:
        file.write(readme_template.format(year_lines = "\n".join(year_lines)))


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
