data = tuple(open('../data/14.txt').read().splitlines())


def roll_rocks(prd):
    prd = tuple(map(''.join, zip(*prd)))
    prd = tuple('#'.join([''.join(sorted(tuple(group), reverse = True)) for group in row.split('#')]) for row in prd)
    return prd


def get_load(prd):
    return sum(row.count('O') * (len(prd) - r) for r, row in enumerate(prd))


def cycle(prd):
    for _ in range(4):
        prd = tuple(row[::-1] for row in roll_rocks(prd))
    return prd


def solve_part_one(prd):
    return get_load(tuple(row[::-1] for row in tuple(map(''.join, zip(*roll_rocks(prd))))))


def solve_part_two(prd):
    seen, arr, i = {prd}, [prd], 0
    while True:
        i += 1
        prd = cycle(prd)
        if prd in seen:
            break
        seen.add(prd)
        arr.append(prd)
    first = arr.index(prd)
    prd = arr[(1000000000 - first) % (i - first) + first]
    return get_load(prd)


print('Part One: %d' % solve_part_one(data))
print('Part Two: %d' % solve_part_two(data))
