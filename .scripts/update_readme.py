import argparse
import colorsys
import json
import os
import time
from datetime import datetime

import requests

LANGUAGE_DETAILS = {
    2015: {
        'label': 'R',
        'logo': 'R',
        'color': '198CE7'
    },
    2023: {
        'label': 'C#',
        'logo': 'https://raw.githubusercontent.com/Asaranth/AdventOfCode/main/.assets/csharp.svg',
        'color': '178600'
    },
    2016: {
        'label': 'Python',
        'logo': 'Python',
        'color': '3572A5'
    },
    2024: {
        'label': 'F#',
        'logo': 'fsharp',
        'color': 'b845fc'
    }
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
    stars_formatted = f"{stars:02d}"
    return f"https://img.shields.io/badge/{year}-{stars_formatted}%20{STAR}-{color}?style=flat-square&labelColor=2b2b2b"


def fmt_language_badge(language: dict) -> str:
    label = language['label']
    logo_url = language['logo']
    color = language['color']
    if "://" in logo_url:
        logo = f"&logo={logo_url}"
    else:
        logo = f"&logo={logo_url}"
    return f"https://img.shields.io/badge/-{label}-{color}?style=flat-square&labelColor=2b2b2b{logo}"


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


def get_language_badge_url(year: int) -> str:
    if year not in LANGUAGE_DETAILS:
        return ""
    return f'<img src="{fmt_language_badge(LANGUAGE_DETAILS[year])}"></img>'


def get_total_badge_url(stars: int) -> str:
    color = hsv_interp(stars / (NUM_YEARS * 50))
    return f'<a href="./README.md"><img src="{fmt_total_badge(stars, color)}"></img></a>'


def main(args):
    y2s = {y: get_year_stars(y, args.sleep_sec) for y in args.years}

    if args.total_only:
        print(get_total_badge_url(sum(y2s.values())))
    else:
        for y, s in y2s.items():
            print(f"{get_year_badge_url(y, s)}<br>", end = '')
            if y in LANGUAGE_DETAILS and s > 0:
                print(f"{get_language_badge_url(y)}<br>", end = '')

    readme_template = """# Advent of Code 🎄

Each year in December, the advent calendar with a twist opens! Advent of Code is an annual event where puzzles are released each day from December 1st to December 25th. Created by Eric Wastl, these puzzles cover a variety of programming aspects, encouraging creative problem-solving and improving coding skills. Join the [fun and educational journey](https://adventofcode.com/)!

## Years

{year_lines}
"""

    year_lines = []
    for year in args.years:
        line = get_year_badge_url(year, y2s[year])
        if y2s[year] > 0 and year in LANGUAGE_DETAILS:
            line += f' {get_language_badge_url(year)}'
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
