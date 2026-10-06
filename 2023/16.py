"""
Day 16: The Floor Will Be Lava

Traces light beam reflections and splitters through an optical contraption grid to count energised tiles.
"""

from collections import deque
from utils import get_input_data

data = get_input_data(16).splitlines()


def fire_beam(r, c, dr, dc):
    """
    Traces beam propagation starting from an initial position and direction, returning the count of energised tiles.

    :param r: Starting row coordinate (may begin off-grid).
    :param c: Starting column coordinate (may begin off-grid).
    :param dr: Row direction delta (-1, 0, 1).
    :param dc: Column direction delta (-1, 0, 1).
    :return: Total number of unique energised grid tile coordinates.
    """
    beam = [(r, c, dr, dc)]
    seen = set()
    queue = deque(beam)

    def add(val):
        if val not in seen:
            seen.add(val)
            queue.append(val)

    while queue:
        r, c, dr, dc = queue.popleft()
        r += dr
        c += dc
        if r < 0 or r >= len(data) or c < 0 or c >= len(data[0]):
            continue
        ch = data[r][c]
        if ch == '.' or (ch == '-' and dc != 0) or (ch == '|' and dr != 0):
            add((r, c, dr, dc))
        elif ch == '/':
            dr, dc = -dc, -dr
            add((r, c, dr, dc))
        elif ch == '\\':
            dr, dc = dc, dr
            add((r, c, dr, dc))
        else:
            for dr, dc in [(1, 0), (-1, 0)] if ch == '|' else [(0, 1), (0, -1)]:
                add((r, c, dr, dc))
    return len({(r, c) for (r, c, _, _) in seen})


def solve_part_one():
    """
    Solves Part One: computes total energised tiles for a beam entering top-left heading right.

    :return: Number of energised tiles.
    """
    return fire_beam(0, -1, 0, 1)


def solve_part_two():
    """
    Solves Part Two: tests all possible perimeter entry points and headings to find maximum energisation.

    :return: Maximum number of energised tiles possible from any edge starting position.
    """
    max_val = 0
    for r in range(len(data)):
        max_val = max(max_val, fire_beam(r, -1, 0, 1))
        max_val = max(max_val, fire_beam(r, len(data[0]), 0, -1))
    for c in range(len(data)):
        max_val = max(max_val, fire_beam(-1, c, 1, 0))
        max_val = max(max_val, fire_beam(len(data), c, -1, 0))
    return max_val


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
