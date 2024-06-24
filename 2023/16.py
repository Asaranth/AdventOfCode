from collections import deque

data = open('data/16.txt').read().splitlines()


def fire_beam(r, c, dr, dc):
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
    return fire_beam(0, -1, 0, 1)


def solve_part_two():
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
