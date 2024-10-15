#!/bin/bash

set -e

SID=${AOC_SESSION_COOKIE:?Environment variable AOC_SESSION_COOKIE is missing}
AOC_UID=${AOC_USER_ID:?Environment variable AOC_USER_ID is missing}
AOC_URL="https://adventofcode.com/%d/leaderboard/private/view/${AOC_UID}.json"
NUM_YEARS=$(( $(date +%Y) - 2015 + 1))
STAR="⭐"

declare -A LANGUAGE_DETAILS
LANGUAGE_DETAILS[2015]="R 198CE7 R"
LANGUAGE_DETAILS[2016]="C# 178600 data:image/svg%2bxml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAxMjggMTI4Ij4NCiAgPHBhdGgNCiAgICBkPSJNMTE3LjUgMzMuNWwuMy0uMmMtLjYtMS4xLTEuNS0yLjEtMi40LTIuNkw2Ny4xIDIuOWMtLjgtLjUtMS45LS43LTMuMS0uNy0xLjIgMC0yLjMuMy0zLjEuN2wtNDggMjcuOWMtMS43IDEtMi45IDMuNS0yLjkgNS40djU1LjdjMCAxLjEuMiAyLjMuOSAzLjRsLS4yLjFjLjUuOCAxLjIgMS41IDEuOSAxLjlsNDguMiAyNy45Yy44LjUgMS45LjcgMy4xLjcgMS4yIDAgMi4zLS4zIDMuMS0uN2w0OC0yNy45YzEuNy0xIDIuOS0zLjUgMi45LTUuNFYzNi4xYy4xLS44IDAtMS43LS40LTIuNnptLTUzLjUgNzBjLTIxLjggMC0zOS41LTE3LjctMzkuNS0zOS41UzQyLjIgMjQuNSA2NCAyNC41YzE0LjcgMCAyNy41IDguMSAzNC4zIDIwbC0xMyA3LjVDODEuMSA0NC41IDczLjEgMzkuNSA2NCAzOS41Yy0xMy41IDAtMjQuNSAxMS0yNC41IDI0LjVzMTEgMjQuNSAyNC41IDI0LjVjOS4xIDAgMTcuMS01IDIxLjMtMTIuNGwxMi45IDcuNmMtNi44IDExLjgtMTkuNiAxOS44LTM0LjIgMTkuOHpNMTE1IDYyaC0zLjJsLS45IDRoNC4xdjVoLTVsLTEuMiA2aC00LjlsMS4yLTZoLTMuOGwtMS4yIDZoLTQuOGwxLjItNkg5NHYtNWgzLjVsLjktNEg5NHYtNWg1LjNsMS4yLTZoNC45bC0xLjIgNmgzLjhsMS4yLTZoNC44bC0xLjIgNmgyLjJ2NXptLTEyLjcgNGgzLjhsLjktNGgtMy44eiINCiAgICBmaWxsPSIjRkZGIiAvPg0KPC9zdmc+"
LANGUAGE_DETAILS[2023]="Python 3572A5 Python"
LANGUAGE_DETAILS[2024]="F# b845fc fsharp"

function rgb2hex {
    local r=$(awk "BEGIN {print int($1 * 255 + 0.5)}")
    local g=$(awk "BEGIN {print int($2 * 255 + 0.5)}")
    local b=$(awk "BEGIN {print int($3 * 255 + 0.5)}")
    printf '%02x%02x%02x' "$r" "$g" "$b"
}

function hsv_interp {
    local t=$1
    local h=$(awk "BEGIN {print $t * 120 / 360}")
    local s=1.0
    local v=0.6
    local r g b
    read r g b < <(awk "BEGIN {
        h = $h;
        s = $s;
        v = $v;
        i = int(h / 60) % 6;
        f = h / 60 - i;
        p = v * (1 - s);
        q = v * (1 - f * s);
        t = v * (1 - (1 - f) * s);

        if (i == 0) r = v; g = t; b = p;
        else if (i == 1) r = q; g = v; b = p;
        else if (i == 2) r = p; g = v; b = t;
        else if (i == 3) r = p; g = q; b = v;
        else if (i == 4) r = t; g = p; b = v;
        else r = v; g = p; b = q;
        print r, g, b;
    }")
    rgb2hex "$r" "$g" "$b"
}

function fmt_year_badge {
    local year=$1
    local stars=$2
    local color=$3
    stars_formatted=$(printf '%02d' "$stars")
    echo "https://img.shields.io/badge/${year}-${stars_formatted}%20${STAR}-${color}?style=for-the-badge&labelColor=2b2b2b"
}

function fmt_language_badge {
    IFS=' ' read -r -a language <<< "${LANGUAGE_DETAILS[$1]}"
    label=${language[0]}
    color=${language[1]}
    logo=${language[2]}
    echo "https://img.shields.io/badge/-${label}-${color}?style=for-the-badge&labelColor=2b2b2b&logo=${logo}&logoColor=white"
}

function get_year_stars {
    local year=$1
    local sleep_sec=$2
    local url=$(printf "$AOC_URL" "$year")
    local res=$(curl -s -H "User-Agent: https://github.com/Asaranth/AdventOfCode/blob/main/.scripts/update_readme.sh" \
                     --cookie "session=${SID}" \
                     "$url")
    sleep "$sleep_sec"
    echo "$res" | jq -r ".members[\"${AOC_UID}\"].stars"
}

function get_year_badge_url {
    local year=$1
    local stars=$2
    local color=$(hsv_interp $(awk "BEGIN {print $stars / 50}"))
    echo "<img src=\"$(fmt_year_badge "$year" "$stars" "$color")\"/>"
}

function get_language_badge_url {
    local year=$1
    if [[ ! -z "${LANGUAGE_DETAILS[$year]}" ]]; then
        echo "<img src=\"$(fmt_language_badge "$year")\"/>"
    fi
}

function main {
    local sleep_sec=2
    local years=($(seq $(date +%Y) -1 2015))

    declare -A y2s
    for year in "${years[@]}"; do
        y2s[$year]=$(get_year_stars "$year" "$sleep_sec")
    done

    local year_lines=()
    for year in "${years[@]}"; do
        year_line=$(get_year_badge_url "$year" "${y2s[$year]}")
        if [[ "${y2s[$year]}" -gt 0 ]]; then
            if [[ ! -z "${LANGUAGE_DETAILS[$year]}" ]]; then
                year_line+=" $(get_language_badge_url "$year")"
            fi
        fi
        year_lines+=("$year_line")
    done

    local readme_template="# Advent of Code 🎄
Each year in December, the advent calendar with a twist opens! Advent of Code is an annual event where puzzles are released each day from December 1st to December 25th. Created by Eric Wastl, these puzzles cover a variety of programming aspects, encouraging creative problem-solving and improving coding skills. Join the [fun and educational journey](https://adventofcode.com/)!
## Years
$(IFS=$'\n'; echo "${year_lines[*]}")"

    echo "$readme_template" > README.md
}

main