"""
Day 06: Wait For It

Calculates the number of winning boat button hold times across races.
"""

from utils import get_input_data

data = get_input_data(6).splitlines()
times, distances = [list(map(int, line.split(':')[1].split())) for line in data]


def get_win_conditions(time, distance):
    """
    Calculates the number of integer hold times that result in travelling farther than the record distance.

    :param time: Total race duration.
    :param distance: Record distance to beat.
    :return: Total number of valid button hold durations.
    """
    win_conditions = 0
    for hold in range(time):
        if hold * (time - hold) > distance:
            win_conditions += 1
    return win_conditions


def solve_part_one():
    """
    Solves Part One: multiplies the count of winning options across all individual races.

    :return: Product of winning possibilities across races.
    """
    total_win_conditions = 1
    for time, distance in zip(times, distances):
        total_win_conditions *= get_win_conditions(time, distance)
    return total_win_conditions


def solve_part_two():
    """
    Solves Part Two: computes winning hold times for a single long race with concatenated digits.

    :return: Number of winning hold times for the single concatenated race.
    """
    time = int(''.join(map(str, times)))
    distance = int(''.join(map(str, distances)))
    return get_win_conditions(time, distance)


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
