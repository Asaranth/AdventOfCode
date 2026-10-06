"""
Day 22: Sand Slabs

Simulates 3D brick settling on a z-buffer stack and models cascading support chain reactions.
"""

from collections import deque
from copy import deepcopy
from utils import get_input_data

data = [list(map(int, line.replace('~', ',').split(','))) for line in get_input_data(22).splitlines()]


def overlaps(a, b):
    """
    Checks whether the horizontal (x, y) projections of two 3D bricks overlap.

    :param a: 6-element integer coordinate list [x1, y1, z1, x2, y2, z2].
    :param b: 6-element integer coordinate list [x1, y1, z1, x2, y2, z2].
    :return: True if horizontal bounds intersect; otherwise, False.
    """
    return max(a[0], b[0]) <= min(a[3], b[3]) and max(a[1], b[1]) <= min(a[4], b[4])


def drop_bricks(bricks):
    """
    Simulates all bricks falling downwards until resting on the ground or on top of previously settled bricks.

    :param bricks: List of 6-element brick coordinate definitions.
    :return: List of settled bricks sorted by z position.
    """
    bricks.sort(key = lambda brick: brick[2])
    for i, brick in enumerate(bricks):
        max_z = 1
        for check in bricks[:i]:
            if overlaps(brick, check):
                max_z = max(max_z, check[5] + 1)
        brick[5] -= brick[2] - max_z
        brick[2] = max_z
    bricks.sort(key = lambda brick: brick[2])
    return bricks


def get_supporting(bricks):
    """
    Constructs support relationship dictionaries indicating which bricks support or are supported by others.

    :param bricks: Settled brick list.
    :return: Tuple of (supports_v, supports_k) mapping brick indices to sets of supported/supporting bricks.
    """
    supports_v = {i: set() for i in range(len(bricks))}
    supports_k = {i: set() for i in range(len(bricks))}
    for j, upper in enumerate(bricks):
        for i, lower in enumerate(bricks[:j]):
            if overlaps(lower, upper) and upper[2] == lower[5] + 1:
                supports_v[i].add(j)
                supports_k[j].add(i)
    return supports_v, supports_k


def solve_part_one():
    """
    Solves Part One: counts how many bricks can be safely disintegrated without causing any other brick to fall.

    :return: Number of safely disintegrable bricks.
    """
    bricks = drop_bricks(deepcopy(data))
    supports_v, supports_k = get_supporting(bricks)
    safe_to_disintegrate = 0
    for i in range(len(bricks)):
        if all(len(supports_k[j]) >= 2 for j in supports_v[i]):
            safe_to_disintegrate += 1
    return safe_to_disintegrate


def solve_part_two():
    """
    Solves Part Two: calculates the sum of falling brick counts caused by disintegrating each brick individually.

    :return: Total sum of cascading falling bricks across all individual disintegrations.
    """
    bricks = drop_bricks(deepcopy(data))
    supports_v, supports_k = get_supporting(bricks)
    total = 0
    for i in range(len(bricks)):
        queue = deque(j for j in supports_v[i] if len(supports_k[j]) == 1)
        falling = set(queue)
        falling.add(i)
        while queue:
            j = queue.popleft()
            for k in supports_v[j] - falling:
                if supports_k[k] <= falling:
                    queue.append(k)
                    falling.add(k)
        total += len(falling) - 1
    return total


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
