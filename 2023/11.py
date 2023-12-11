data = open('data/11.txt').read().splitlines()
empty_rows = [r for r, row in enumerate(data) if '#' not in row]
empty_cols = [c for c in range(len(data[0])) if '#' not in [row[c] for row in data]]
galaxies = {(r, c) for r, row in enumerate(data) for c, ch in enumerate(row) if ch == '#'}


def get_distance(g1, g2, es_mult):
    distance_y = abs(g1[0] - g2[0])
    distance_x = abs(g1[1] - g2[1])
    for r in empty_rows:
        if g1[0] < r < g2[0] or g2[0] < r < g1[0]:
            distance_y += es_mult
    for c in empty_cols:
        if g1[1] < c < g2[1] or g2[1] < c < g1[1]:
            distance_x += es_mult
    return distance_x + distance_y


def travel_galaxies(empty_space_adjustment):
    total_distance = 0
    for galaxy in galaxies:
        for next_galaxy in galaxies:
            if galaxy != next_galaxy:
                total_distance += get_distance(galaxy, next_galaxy, empty_space_adjustment)
    return total_distance // 2


def solve_part_one():
    return travel_galaxies(1)  # Empty space is twice as large


def solve_part_two():
    return travel_galaxies(999999)  # Empty space is 1 million times larger


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())