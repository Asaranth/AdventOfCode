"""
Day 01: Trebuchet?!

Extracts calibration values from string lines by scanning first and last digits,
including spelled-out digit words.
"""

from re import findall
from utils import get_input_data

data = get_input_data(1).splitlines()
number_map = {'one': 1, 'two': 2, 'three': 3, 'four': 4, 'five': 5, 'six': 6, 'seven': 7, 'eight': 8, 'nine': 9}
pattern = r'(?=(' + '|'.join(list(number_map.keys()) + list(map(str, number_map.values()))) + '))'


def get_calibration_value(numbers):
    """
    Combines the first and last digits of a sequence into a two-digit integer.

    :param numbers: List of matched digit strings or numeric values.
    :return: Two-digit calibration value.
    """
    if len(numbers) == 1:
        return int('{0}{0}'.format(numbers[0]))
    return int('%s%s' % (numbers[0], numbers[-1]))


def words_to_numbers(line):
    """
    Parses a line using overlapping regex to extract spelled-out words and digits, returning calibration value.

    :param line: Raw text line containing alphanumeric characters.
    :return: Two-digit calibration integer.
    """
    numbers = [number_map.get(match, match) for match in findall(pattern, line)]
    return get_calibration_value(numbers)


def solve_part_one():
    """
    Solves Part One: extracts numeric digits only and sums all calibration values.

    :return: Sum of all calibration values.
    """
    results = []
    for line in data:
        results.append(get_calibration_value(findall(r'\d', line)))
    return sum(results)


def solve_part_two():
    """
    Solves Part Two: extracts both digit words and numeric digits and sums calibration values.

    :return: Sum of all calibration values under revised parsing rules.
    """
    results = []
    for line in data:
        results.append(words_to_numbers(line))
    return sum(results)


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
