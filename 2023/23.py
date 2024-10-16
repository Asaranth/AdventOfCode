from utils import get_input_data

data = get_input_data(23).splitlines()
start = (0, data[0].index('.'))
end = (len(data) - 1, data[-1].index('.'))
directions = {
    '^': [(-1, 0)],
    'v': [(1, 0)],
    '<': [(0, -1)],
    '>': [(0, 1)],
    '.': [(-1, 0), (1, 0), (0, -1), (0, 1)]
}


def get_points():
    points = [start, end]
    for r, row in enumerate(data):
        for c, ch in enumerate(row):
            if ch == '#':
                continue
            neighbors = 0
            for nr, nc in [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]:
                if 0 <= nr < len(data) and 0 <= nc < len(data[0]) and data[nr][nc] != '#':
                    neighbors += 1
                if neighbors >= 3:
                    points.append((r, c))
    return points


def get_trails(part):
    points = get_points()
    graph = {pt: {} for pt in points}
    for sr, sc in points:
        stack = [(0, sr, sc)]
        seen = {(sr, sc)}
        while stack:
            n, r, c = stack.pop()
            if n != 0 and (r, c) in points:
                graph[(sr, sc)][(r, c)] = n
                continue
            for dr, dc in (directions[data[r][c]] if part == 1 else [(-1, 0), (1, 0), (0, -1), (0, 1)]):
                nr = r + dr
                nc = c + dc
                if 0 <= nr < len(data) and 0 <= nc < len(data[0]) and data[nr][nc] != '#' and (nr, nc) not in seen:
                    stack.append((n + 1, nr, nc))
                    seen.add((nr, nc))
    return graph


def solve_part_one():
    trails = get_trails(1)
    seen = set()

    def get_longest_trail(pt):
        if pt == end:
            return 0
        m = -float('inf')
        seen.add(pt)
        for nx in trails[pt]:
            m = max(m, get_longest_trail(nx) + trails[pt][nx])
        seen.remove(pt)
        return m

    return get_longest_trail(start)


def solve_part_two():
    trails = get_trails(2)
    seen = set()

    def get_longest_trail(pt):
        if pt == end:
            return 0
        m = -float('inf')
        seen.add(pt)
        for nx in trails[pt]:
            if nx not in seen:
                m = max(m, get_longest_trail(nx) + trails[pt][nx])
        seen.remove(pt)
        return m

    return get_longest_trail(start)


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
