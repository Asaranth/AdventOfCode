"""
Day 03: Gear Ratios

Parses engine schematic grids to detect numbers adjacent to symbols and calculate gear ratios.
"""

from utils import get_input_data

data = get_input_data(3).splitlines()


def get_all_coordinates(row, col):
    """
    Finds the starting coordinates of all distinct numbers adjacent to a given grid cell.

    :param row: Row index of the target symbol.
    :param col: Column index of the target symbol.
    :return: Set of (row, col) tuples representing the start positions of adjacent numbers.
    """
    coordinates = set()
    for cr in [row - 1, row, row + 1]:
        for cc in [col - 1, col, col + 1]:
            if cr < 0 or cr >= len(data) or cc < 0 or cc >= len(data[cr]) or not data[cr][cc].isdigit():
                continue
            while cc > 0 and data[cr][cc - 1].isdigit():
                cc -= 1
            coordinates.add((cr, cc))
    return coordinates


def get_numbers(coordinates):
    """
    Extracts the full integer values starting at each given coordinate.

    :param coordinates: Iterable of (row, col) starting positions of numbers.
    :return: List of parsed integer values.
    """
    numbers = []
    for x, y in coordinates:
        s = ''
        while y < len(data[x]) and data[x][y].isdigit():
            s += data[x][y]
            y += 1
        numbers.append(int(s))
    return numbers


def solve_part_one():
    """
    Solves Part One: sums all numbers adjacent to at least one non-period symbol in the schematic.

    :return: Sum of all valid part numbers.
    """
    coordinates = set()
    for r, row in enumerate(data):
        for c, ch in enumerate(row):
            if ch.isdigit() or ch == '.':
                continue
            coordinates.update(get_all_coordinates(r, c))
    return sum(get_numbers(coordinates))


def solve_part_two():
    """
    Solves Part Two: identifies all gear symbols ('*') adjacent to exactly two numbers and sums their ratios.

    :return: Sum of all gear ratios.
    """
    total = 0
    for r, row in enumerate(data):
        for c, ch in enumerate(row):
            if ch != '*':
                continue
            coordinates = get_all_coordinates(r, c)
            if len(coordinates) != 2:
                continue
            numbers = get_numbers(coordinates)
            total += numbers[0] * numbers[1]
    return total


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
