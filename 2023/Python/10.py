from collections import deque

data = open('../data/10.txt').read().splitlines()


def process_grid(g):
    sr, sc = get_start()
    loop = {(sr, sc)}
    queue = deque([(sr, sc)])
    maybe_s = {'|', '-', 'J', 'L', '7', 'F'}

    while queue:
        r, c = queue.popleft()
        tile = g[r][c]
        if can_go_up(r, c, tile) and (r - 1, c) not in loop:
            loop.add((r - 1, c))
            queue.append((r - 1, c))
            if tile == 'S':
                maybe_s &= {'|', 'J', 'L'}
        if can_go_down(r, c, tile) and (r + 1, c) not in loop:
            loop.add((r + 1, c))
            queue.append((r + 1, c))
            if tile == 'S':
                maybe_s &= {'|', '7', 'F'}
        if can_go_left(r, c, tile) and (r, c - 1) not in loop:
            loop.add((r, c - 1))
            queue.append((r, c - 1))
            if tile == 'S':
                maybe_s &= {'-', 'J', '7'}
        if can_go_right(r, c, tile) and (r, c + 1) not in loop:
            loop.add((r, c + 1))
            queue.append((r, c + 1))
            if tile == 'S':
                maybe_s &= {'-', 'F', 'L'}
    assert len(maybe_s) == 1
    (S,) = maybe_s
    g = [row.replace('S', S) for row in g]
    return [[ch if (r, c) in loop else '.' for c, ch in enumerate(row)] for r, row in enumerate(g)], loop


def can_go_up(row, col, char):
    return row > 0 and char in 'S|JL' and data[row - 1][col] in '|7F'


def can_go_down(row, col, char):
    return row < len(data) - 1 and char in 'S|7F' and data[row + 1][col] in '|JL'


def can_go_left(row, col, char):
    return col > 0 and char in 'S-J7' and data[row][col - 1] in '-FL'


def can_go_right(row, col, char):
    return col < len(data[0]) and char in 'S-FL' and data[row][col + 1] in '-J7'


def get_start():
    for r, row in enumerate(data):
        for c, tile in enumerate(row):
            if tile == 'S':
                return r, c


def get_outside(g):
    outside = set()
    for r, row in enumerate(g):
        within = False
        up = None
        for c, char in enumerate(row):
            if char == '|':
                assert up is None
                within = not within
            elif char == '-':
                assert up is not None
            elif char in 'LF':
                assert up is None
                up = char == 'L'
            elif char in '7J':
                assert up is not None
                if char != ('J' if up else '7'):
                    within = not within
                up = None
            elif char == '.':
                pass
            if not within:
                outside.add((r, c))
    return outside


def solve_part_one(loop):
    return len(loop) // 2


def solve_part_two(g, loop):
    outside = get_outside(g)
    return len(g) * len(g[0]) - len(outside | loop)


grid, pipe_loop = process_grid(data)
print('Part One: %d' % solve_part_one(pipe_loop))
print('Part Two: %d' % solve_part_two(grid, pipe_loop))
