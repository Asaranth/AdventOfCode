data = open('data/18.txt').read().splitlines()
directions = {
    'U': (-1, 0),
    'D': (1, 0),
    'L': (0, -1),
    'R': (0, 1)
}


def get_area(p):
    return abs(sum(p[i][0] * (p[i - 1][1] - p[(i + 1) % len(p)][1]) for i in range(len(p)))) / 2


def picks_theorem(area, boundary):
    return (area - boundary // 2 + 1) + boundary


def solve_part_one():
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
