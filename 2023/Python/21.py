from collections import deque

grid = open('../data/21.txt').read().splitlines()
start_row, start_col = next((r, c) for r, row in enumerate(grid) for c, ch in enumerate(row) if ch == 'S')


def take_steps(sr, sc, steps):
    ans = set()
    seen = {(sr, sc)}
    queue = deque([(sr, sc, steps)])
    while queue:
        r, c, s = queue.popleft()
        if s % 2 == 0:
            ans.add((r, c))
        if s == 0:
            continue
        for nr, nc in [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]:
            if nr < 0 or nr >= len(grid) or nc < 0 or nc >= len(grid[0]) or grid[nr][nc] == '#' or (nr, nc) in seen:
                continue
            seen.add((nr, nc))
            queue.append((nr, nc, s - 1))
    return len(ans)


def full_grids(width, size):
    odd_grids = (width // 2 * 2 + 1) ** 2
    even_grids = ((width + 1) // 2 * 2) ** 2
    odd_points = take_steps(start_row, start_col, size * 2 + 1)
    even_points = take_steps(start_row, start_col, size * 2)
    return odd_grids * odd_points + even_grids * even_points


def grid_points(size):
    top_points = take_steps(size - 1, start_col, size - 1)
    bottom_points = take_steps(0, start_col, size - 1)
    right_points = take_steps(start_row, 0, size - 1)
    left_points = take_steps(start_row, size - 1, size - 1)
    return top_points + bottom_points + right_points + left_points


def small_slices(width, size):
    top_right = take_steps(size - 1, 0, size // 2 - 1)
    top_left = take_steps(size - 1, size - 1, size // 2 - 1)
    bottom_right = take_steps(0, 0, size // 2 - 1)
    bottom_left = take_steps(0, size - 1, size // 2 - 1)
    return (width + 1) * (top_right + top_left + bottom_right + bottom_left)


def big_slices(width, size):
    top_right = take_steps(size - 1, 0, size * 3 // 2 - 1)
    top_left = take_steps(size - 1, size - 1, size * 3 // 2 - 1)
    bottom_right = take_steps(0, 0, size * 3 // 2 - 1)
    bottom_left = take_steps(0, size - 1, size * 3 // 2 - 1)
    return width * (top_right + top_left + bottom_right + bottom_left)


def solve_part_one():
    return take_steps(start_row, start_col, 64)


def solve_part_two():
    size = len(grid)
    steps = 26501365
    assert len(grid) == len(grid[0])
    assert start_row == start_col == size // 2
    assert steps % size == size // 2
    width = steps // size - 1
    return full_grids(width, size) + grid_points(size) + small_slices(width, size) + big_slices(width, size)


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
