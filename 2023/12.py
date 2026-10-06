"""
Day 12: Hot Springs

Calculates valid operational and damaged spring assignments using dynamic programming with memoisation.
"""

from utils import get_input_data

data = get_input_data(12).splitlines()
data = [line.split() for line in data]
memo = {}


def count_arrangements(config, damaged_map):
    """
    Recursively counts the number of valid spring configurations matching the damaged group counts.

    :param config: Remaining spring pattern string (consisting of '.', '#', and '?').
    :param damaged_map: Tuple of remaining contiguous damaged spring group lengths.
    :return: Number of valid arrangements.
    """
    if config == '':
        return 1 if damaged_map == () else 0
    if damaged_map == ():
        return 0 if '#' in config else 1

    key = (config, damaged_map)
    if key in memo:
        return memo[key]

    result = 0
    if config[0] in '.?':
        result += count_arrangements(config[1:], damaged_map)
    if config[0] in '#?':
        if enough_left(config, damaged_map[0]) and all_broken(config, damaged_map[0]) and next_works(config,
                                                                                                     damaged_map[0]):
            result += count_arrangements(config[damaged_map[0] + 1:], damaged_map[1:])

    memo[key] = result
    return result


def enough_left(config, damaged_index):
    """
    Checks if enough characters remain in the configuration to satisfy the current damaged group size.

    :param config: Spring configuration substring.
    :param damaged_index: Target damaged group length.
    :return: True if length is sufficient; otherwise, False.
    """
    return damaged_index <= len(config)


def all_broken(config, damaged_index):
    """
    Checks whether all characters in the target window can represent broken springs (no operational '.' springs).

    :param config: Spring configuration substring.
    :param damaged_index: Target damaged group length.
    :return: True if the prefix contains no '.' characters; otherwise, False.
    """
    return '.' not in config[:damaged_index]


def next_works(config, damaged_index):
    """
    Checks whether the separator following a damaged group is valid (end of string or not '#').

    :param config: Spring configuration substring.
    :param damaged_index: Target damaged group length.
    :return: True if the following character can be a separator; otherwise, False.
    """
    return damaged_index == len(config) or config[damaged_index] != '#'


def unfold(config, damaged_map):
    """
    Unfolds records by replicating the pattern and damaged tuple five times joined by '?'.

    :param config: Base spring pattern string.
    :param damaged_map: Tuple of damaged group lengths.
    :return: Tuple of (unfolded_pattern_string, unfolded_damaged_tuple).
    """
    return '?'.join([config] * 5), damaged_map * 5


def solve_part_one():
    """
    Solves Part One: sums valid arrangement counts for original single-fold records.

    :return: Total valid arrangements.
    """
    total = 0
    for d in data:
        total += count_arrangements(d[0], tuple(map(int, d[1].split(','))))
    return total


def solve_part_two():
    """
    Solves Part Two: sums valid arrangement counts for unfolded five-fold records.

    :return: Total valid arrangements across unfolded records.
    """
    total = 0
    for d in data:
        config, damaged_map = unfold(d[0], tuple(map(int, d[1].split(','))))
        total += count_arrangements(config, damaged_map)
    return total


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
