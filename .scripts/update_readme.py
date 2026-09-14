import argparse
import colorsys
import json
import os
import time
import urllib.request
from datetime import datetime
from pathlib import Path

import requests
import yaml


SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent

LANGUAGE_CONFIG = REPO_ROOT / 'language-badges.yml'
README = REPO_ROOT / 'README.md'

LINGUIST_URL = (
    'https://raw.githubusercontent.com/'
    'github-linguist/linguist/main/lib/linguist/languages.yml'
)

SID = os.getenv('AOC_SESSION_COOKIE')
assert SID is not None

UID = os.getenv('AOC_USER_ID')
assert UID is not None

AOC_URL = 'https://adventofcode.com/{year}/leaderboard/private/view/{uid}.json'

HEADERS = {
    'User-Agent': (
        'https://github.com/Asaranth/AdventOfCode/'
        'blob/main/.scripts/update_readme.py'
    )
}

COOKIES = {'session': SID}

STAR = '⭐'


def load_language_config() -> dict:
    with LANGUAGE_CONFIG.open(encoding='utf-8') as file:
        config = yaml.safe_load(file)

    if not config or 'languages' not in config:
        raise RuntimeError(
            f'{LANGUAGE_CONFIG} does not contain a "languages" section.'
        )

    return config['languages']


def load_linguist() -> dict:
    print('Downloading current GitHub Linguist language definitions...')

    request = urllib.request.Request(
        LINGUIST_URL,
        headers={
            'User-Agent': 'AdventOfCode-README-Updater',
        },
    )

    with urllib.request.urlopen(request) as response:
        return yaml.safe_load(
            response.read().decode('utf-8')
        )


def find_language(name: str, linguist: dict) -> dict:
    if name in linguist:
        return linguist[name]

    name_lower = name.lower()

    for language, data in linguist.items():
        aliases = data.get('aliases', []) or []

        if any(alias.lower() == name_lower for alias in aliases):
            return data

    raise RuntimeError(
        f"'{name}' was not found in GitHub Linguist."
    )


def load_language_details() -> dict:
    languages = load_language_config()
    linguist = load_linguist()

    for details in languages.values():
        language = details['linguist']

        definition = find_language(
            language,
            linguist,
        )

        color = definition.get('color')

        if not color:
            raise RuntimeError(
                f"'{language}' has no GitHub Linguist colour."
            )

        details['color'] = color.lstrip('#')

        print(
            f"{language}: {details['color']}"
        )

    return languages


def rgb2hex(r, g, b):
    f = lambda x: max(0, min(255, round(x * 255)))
    return f'{f(r):02x}{f(g):02x}{f(b):02x}'


def hsv_interp(t):
    assert 0 <= t <= 1

    return rgb2hex(
        *colorsys.hsv_to_rgb(
            h=t * 120 / 360,
            s=1,
            v=0.6,
        )
    )


def fmt_year_badge(
    year: int,
    stars: int,
    max_stars: int,
    color: str,
) -> str:
    stars_formatted = f'{stars:02d}%2F{max_stars:02d}'

    return (
        f'https://img.shields.io/badge/'
        f'{year}-{stars_formatted}%20{STAR}-{color}'
        f'?style=for-the-badge'
        f'&labelColor=2b2b2b'
    )


def fmt_language_badge(language: dict) -> str:
    label = language['label']
    logo = language['logo']
    color = language['color']

    return (
        f'https://img.shields.io/badge/'
        f'-{label}-{color}'
        f'?style=for-the-badge'
        f'&labelColor=2b2b2b'
        f'&logo={logo}'
        f'&logoColor=white'
    )


def get_year_stars(year: int, sleep_sec: int) -> int:
    url = AOC_URL.format(
        year=year,
        uid=UID,
    )

    res = requests.get(
        url,
        headers=HEADERS,
        cookies=COOKIES,
    )

    if res.status_code != 200:
        print(
            f'Error fetching data for year {year}: '
            f'HTTP {res.status_code}: {res.text}'
        )
        raise AssertionError(
            'Failed to fetch leaderboard data'
        )

    time.sleep(sleep_sec)

    data = json.loads(res.text)

    return data['members'][UID]['stars']


def get_year_badge_url(
    year: int,
    stars: int,
) -> str:
    max_stars = 24 if year >= 2025 else 50

    color = hsv_interp(
        stars / max_stars
    )

    badge_url = fmt_year_badge(
        year,
        stars,
        max_stars,
        color,
    )

    return (
        f'<a href="./{year}/">'
        f'<img src="{badge_url}" alt="{year}">'
        f'</a>'
    )


def get_language_badge_url(
    year: int,
    language_details: dict,
) -> str:
    if year not in language_details:
        return ''

    badge_url = fmt_language_badge(
        language_details[year]
    )

    return (
        f'<a href="./{year}/">'
        f'<img src="{badge_url}" '
        f'alt="{language_details[year]["label"]}">'
        f'</a>'
    )


def get_years() -> list:
    current_year = datetime.now().year
    current_month = datetime.now().month

    last_year = (
        current_year
        if current_month >= 12
        else current_year - 1
    )

    return list(
        range(2015, last_year + 1)
    )[::-1]


def main(args):
    language_details = load_language_details()

    y2s = {
        year: get_year_stars(
            year,
            args.sleep_sec,
        )
        for year in args.years
    }

    total_stars = sum(y2s.values())

    readme_template = """
# Stars: {total_stars} ⭐
{year_lines}
"""

    year_lines = []

    for year in args.years:
        line = get_year_badge_url(
            year,
            y2s[year],
        )

        if (
            y2s[year] > 0
            and year in language_details
        ):
            line += (
                ' '
                + get_language_badge_url(
                    year,
                    language_details,
                )
            )

        year_lines.append(line)

    with README.open(
        'w',
        encoding='utf-8',
    ) as file:
        file.write(
            readme_template.format(
                total_stars=total_stars,
                year_lines='<br>\n'.join(
                    year_lines
                ),
            )
        )


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description=(
            'Generate badge URLs with stars/year. '
            'The badge color is interpolated with respect '
            'to the number of stars: from 0 to 50 '
            '(or 24 for 2025+).'
        ),
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )

    parser.add_argument(
        '--years',
        nargs='+',
        type=int,
        default=get_years(),
        help='Years to fetch data from.',
    )

    parser.add_argument(
        '--sleep-sec',
        type=int,
        default=2,
        help='Number of seconds to sleep between requests.',
    )

    args = parser.parse_args()

    main(args)
