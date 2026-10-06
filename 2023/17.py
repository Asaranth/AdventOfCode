"""
Day 17: Clumsy Crucible

Calculates minimum heat loss paths using Dijkstra's algorithm subject to straight-line movement constraints.
"""

from heapq import heappush, heappop
from utils import get_input_data

data = [list(map(int, line.strip())) for line in get_input_data(17).splitlines()]
rows, cols = len(data), len(data[0])


def dijkstras_algorithm(min_steps_before_turn, max_steps_before_turn):
    """
    Finds the minimum cumulative heat loss to reach the bottom-right corner using Dijkstra's algorithm.

    :param min_steps_before_turn: Minimum consecutive steps in the current direction before turning is allowed.
    :param max_steps_before_turn: Maximum consecutive steps allowed in the same direction.
    :return: Minimum heat loss to reach destination.
    """
    seen = set()
    priority_queue = [(0, 0, 0, 0, 0, 0)]
    while priority_queue:
        heat_loss, r, c, dr, dc, steps = heappop(priority_queue)
        if r == len(data) - 1 and c == len(data[0]) - 1 and steps >= min_steps_before_turn:
            return heat_loss
        if r < 0 or r >= len(data) or c < 0 or c >= len(data[0]):
            continue
        if (r, c, dr, dc, steps) in seen:
            continue
        seen.add((r, c, dr, dc, steps))
        if steps < max_steps_before_turn and (dr, dc) != (0, 0):
            nr = r + dr
            nc = c + dc
            if 0 <= nr < len(data) and 0 <= nc < len(data[0]):
                heappush(priority_queue, (heat_loss + data[nr][nc], nr, nc, dr, dc, steps + 1))
        if steps >= min_steps_before_turn or (dr, dc) == (0, 0):
            for ndr, ndc in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                if (ndr, ndc) != (dr, dc) and (ndr, ndc) != (-dr, -dc):
                    nr = r + ndr
                    nc = c + ndc
                    if 0 <= nr < len(data) and 0 <= nc < len(data[0]):
                        heappush(priority_queue, (heat_loss + data[nr][nc], nr, nc, ndr, ndc, 1))


def solve_part_one():
    """
    Solves Part One: finds minimum heat loss for a standard crucible (1 to 3 steps per direction).

    :return: Minimum heat loss.
    """
    return dijkstras_algorithm(1, 3)


def solve_part_two():
    """
    Solves Part Two: finds minimum heat loss for an ultra crucible (4 to 10 steps per direction).

    :return: Minimum heat loss.
    """
    return dijkstras_algorithm(4, 10)


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
