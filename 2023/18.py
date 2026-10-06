"""
Day 18: Lavaduct Lagoon

Computes polygonal trench capacity using the Shoelace formula and Pick's theorem on integer coordinates.
"""

from utils import get_input_data

data = get_input_data(18).splitlines()
directions = {
    'U': (-1, 0),
    'D': (1, 0),
    'L': (0, -1),
    'R': (0, 1)
}


def get_area(p):
    """
    Computes polygon area from a list of vertices using the Shoelace formula.

    :param p: List of (row, col) polygon vertices.
    :return: Enclosed polygon area.
    """
    return abs(sum(p[i][0] * (p[i - 1][1] - p[(i + 1) % len(p)][1]) for i in range(len(p)))) / 2


def picks_theorem(area, boundary):
    """
    Calculates total enclosed integer grid points (interior plus boundary) using Pick's theorem.

    :param area: Polygonal area calculated via Shoelace formula.
    :param boundary: Total perimeter boundary length.
    :return: Total number of interior and boundary lagoon tiles.
    """
    return (area - boundary // 2 + 1) + boundary


def solve_part_one():
    """
    Solves Part One: computes lagoon capacity using standard direction and metre distance instructions.

    :return: Total lagoon capacity in cubic metres.
    """
    points = [(0, 0)]
    boundary = 0
    for line in data:
        direction, distance, _ = line.split()
        dr, dc = directions[direction]
        distance = int(distance)
        boundary += distance
        r, c = points[-1]
        points.append((r + dr * distance, c + dc * distance))
    return picks_theorem(get_area(points), boundary)


def solve_part_two():
    """
    Solves Part Two: computes lagoon capacity by decoding hexadecimal colour codes into distances and directions.

    :return: Total lagoon capacity under hexadecimal instruction decoding.
    """
    points = [(0, 0)]
    boundary = 0
    for line in data:
        _, _, x = line.split()
        x = x[2:-1]
        dr, dc = directions['RDLU'[int(x[-1])]]
        distance = int(x[:-1], 16)
        boundary += distance
        r, c = points[-1]
        points.append((r + dr * distance, c + dc * distance))
    return picks_theorem(get_area(points), boundary)


print('Part One %d' % solve_part_one())
print('Part Two %d' % solve_part_two())
