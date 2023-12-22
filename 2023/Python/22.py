from collections import deque
from copy import deepcopy

data = [list(map(int, line.replace('~', ',').split(','))) for line in open('../data/22.txt')]


def overlaps(a, b):
    return max(a[0], b[0]) <= min(a[3], b[3]) and max(a[1], b[1]) <= min(a[4], b[4])


def drop_bricks(bricks):
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
    supports_v = {i: set() for i in range(len(bricks))}
    supports_k = {i: set() for i in range(len(bricks))}
    for j, upper in enumerate(bricks):
        for i, lower in enumerate(bricks[:j]):
            if overlaps(lower, upper) and upper[2] == lower[5] + 1:
                supports_v[i].add(j)
                supports_k[j].add(i)
    return supports_v, supports_k


def solve_part_one():
    bricks = drop_bricks(deepcopy(data))
    supports_v, supports_k = get_supporting(bricks)
    safe_to_disintegrate = 0
    for i in range(len(bricks)):
        if all(len(supports_k[j]) >= 2 for j in supports_v[i]):
            safe_to_disintegrate += 1
    return safe_to_disintegrate


def solve_part_two():
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
