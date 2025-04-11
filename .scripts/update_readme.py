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
        'logo': 'r',
        'color': '198CE7'
    },
    2016: {
        'label': 'C%23',
        'logo': 'data:image/svg%2bxml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAxMjggMTI4Ij4NCiAgPHBhdGgNCiAgICBkPSJNMTE3LjUgMzMuNWwuMy0uMmMtLjYtMS4xLTEuNS0yLjEtMi40LTIuNkw2Ny4xIDIuOWMtLjgtLjUtMS45LS43LTMuMS0uNy0xLjIgMC0yLjMuMy0zLjEuN2wtNDggMjcuOWMtMS43IDEtMi45IDMuNS0yLjkgNS40djU1LjdjMCAxLjEuMiAyLjMuOSAzLjRsLS4yLjFjLjUuOCAxLjIgMS41IDEuOSAxLjlsNDguMiAyNy45Yy44LjUgMS45LjcgMy4xLjcgMS4yIDAgMi4zLS4zIDMuMS0uN2w0OC0yNy45YzEuNy0xIDIuOS0zLjUgMi45LTUuNFYzNi4xYy4xLS44IDAtMS43LS40LTIuNnptLTUzLjUgNzBjLTIxLjggMC0zOS41LTE3LjctMzkuNS0zOS41UzQyLjIgMjQuNSA2NCAyNC41YzE0LjcgMCAyNy41IDguMSAzNC4zIDIwbC0xMyA3LjVDODEuMSA0NC41IDczLjEgMzkuNSA2NCAzOS41Yy0xMy41IDAtMjQuNSAxMS0yNC41IDI0LjVzMTEgMjQuNSAyNC41IDI0LjVjOS4xIDAgMTcuMS01IDIxLjMtMTIuNGwxMi45IDcuNmMtNi44IDExLjgtMTkuNiAxOS44LTM0LjIgMTkuOHpNMTE1IDYyaC0zLjJsLS45IDRoNC4xdjVoLTVsLTEuMiA2aC00LjlsMS4yLTZoLTMuOGwtMS4yIDZoLTQuOGwxLjItNkg5NHYtNWgzLjVsLjktNEg5NHYtNWg1LjNsMS4yLTZoNC45bC0xLjIgNmgzLjhsMS4yLTZoNC44bC0xLjIgNmgyLjJ2NXptLTEyLjcgNGgzLjhsLjktNGgtMy44eiINCiAgICBmaWxsPSIjRkZGIiAvPg0KPC9zdmc+',
        'color': '178600'
    },
    2017: {
        'label': 'F%23',
        'logo': 'fsharp',
        'color': 'b845fc'
    },
    2018: {
        'label': 'JavaScript',
        'logo': 'javascript',
        'color': 'f1e05a'
    },
    2023: {
        'label': 'Python',
        'logo': 'python',
        'color': '3572A5'
    },
    2024: {
        'label': 'Rust',
        'logo': 'rust',
        'color': 'dea584'
    }
}

SID = os.getenv('AOC_SESSION_COOKIE')
assert SID is not None

UID = os.getenv('AOC_USER_ID')
assert UID is not None

AOC_URL = 'https://adventofcode.com/{year}/leaderboard/private/view/{uid}.json'
HEADERS = {
    'User-Agent': 'https://github.com/Asaranth/AdventOfCode/blob/main/.scripts/update_readme.py'
}
COOKIES = {'session': SID}
STAR = '⭐'
NUM_YEARS = datetime.now().year - 2015 + 1


def rgb2hex(r, g, b):
    f = lambda x: max(0, min(255, round(x * 255)))
    return f'{f(r):02x}{f(g):02x}{f(b):02x}'


def hsv_interp(t):
    assert 0 <= t <= 1
    return rgb2hex(*colorsys.hsv_to_rgb(h = t * 120 / 360, s = 1, v = 0.6))


def fmt_year_badge(year: int, stars: int, color: str) -> str:
    stars_formatted = f'{stars:02d}'
    return f'https://img.shields.io/badge/{year}-{stars_formatted}%20{STAR}-{color}?style=for-the-badge&labelColor=2b2b2b'


def fmt_language_badge(language: dict) -> str:
    label = language['label']
    logo = language['logo']
    color = language['color']
    return f'https://img.shields.io/badge/-{label}-{color}?style=for-the-badge&labelColor=2b2b2b&logo={logo}&logoColor=white'


def get_year_stars(year: int, sleep_sec: int) -> int:
    res = requests.get(
        AOC_URL.format(year = year, uid = UID),
        headers = HEADERS,
        cookies = COOKIES,
    )
    assert res.status_code == 200
    time.sleep(sleep_sec)
    data = json.loads(res.text)
    return data['members'][UID]['stars']


def get_year_badge_url(year: int, stars: int) -> str:
    color = hsv_interp(stars / 50)
    badge_url = fmt_year_badge(year, stars, color)
    return f'<a href="./{year}/"><img src="{badge_url}" alt="{year}"></a>'


def get_language_badge_url(year: int) -> str:
    if year not in LANGUAGE_DETAILS:
        return ''
    badge_url = fmt_language_badge(LANGUAGE_DETAILS[year])
    return f'<a href="./{year}/"><img src="{badge_url}" alt="{LANGUAGE_DETAILS[year]["label"]}"></a>'


def main(args):
    y2s = {y: get_year_stars(y, args.sleep_sec) for y in args.years}
    total_stars = sum(y2s.values())

    for y, s in y2s.items():
        get_year_badge_url(y, s)
        if y in LANGUAGE_DETAILS and s > 0:
            get_language_badge_url(y)


    readme_template = """# Advent of Code 🎄
Each year in December, the advent calendar with a twist opens! Advent of Code is an annual event where puzzles are released each day from December 1st to December 25th. Created by Eric Wastl, these puzzles cover a variety of programming aspects, encouraging creative problem-solving and improving coding skills. Join the [fun and educational journey](https://adventofcode.com/)!
## Stars: {total_stars} ⭐
{year_lines}
"""

    year_lines = []
    for year in args.years:
        line = get_year_badge_url(year, y2s[year])
        if y2s[year] > 0 and year in LANGUAGE_DETAILS:
            line += f' {get_language_badge_url(year)}'
        year_lines.append(line)

    with open('README.md', 'w', encoding = 'utf-8') as file:
        file.write(readme_template.format(total_stars = total_stars, year_lines = '<br>\n'.join(year_lines)))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description = '''Generate badge URLs with stars/year.
        The badge color is interpolated with respect to the number of stars: from 0 to 50.''',
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        '--years',
        nargs = '+',
        type = int,
        default = list(range(datetime.now().year, 2014, -1)),
        help = 'Years to fetch data from.',
    )
    parser.add_argument(
        '--sleep-sec',
        type = int,
        default = 2,
        help = 'Number of seconds to sleep between requests.',
    )
    args = parser.parse_args()
    main(args)