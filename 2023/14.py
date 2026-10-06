"""
Day 14: Parabolic Reflector Dish

Simulates rolling rocks on a tilted reflector dish and detects state cycles to compute north support beam load.
"""

from utils import get_input_data

data = tuple(get_input_data(14).splitlines())


def roll_rocks(prd):
    """
    Tilts the grid so that all rounded rocks roll towards the north edge.

    :param prd: Tuple of strings representing grid rows.
    :return: Transformed grid tuple with rolled rocks.
    """
    prd = tuple(map(''.join, zip(*prd)))
    prd = tuple('#'.join([''.join(sorted(tuple(group), reverse = True)) for group in row.split('#')]) for row in prd)
    return prd


def get_load(prd):
    """
    Calculates total load on the north support beam by weighting each rounded rock by its distance from south edge.

    :param prd: Grid row tuple.
    :return: Total accumulated beam load.
    """
    return sum(row.count('O') * (len(prd) - r) for r, row in enumerate(prd))


def cycle(prd):
    """
    Performs one complete spin cycle (tilting North, West, South, East).

    :param prd: Initial grid tuple.
    :return: Resulting grid tuple after four orthogonal tilts.
    """
    for _ in range(4):
        prd = tuple(row[::-1] for row in roll_rocks(prd))
    return prd


def solve_part_one(prd):
    """
    Solves Part One: tilts rocks north once and computes the total north load.

    :param prd: Initial grid tuple.
    :return: Total load on the north support beam.
    """
    return get_load(tuple(row[::-1] for row in tuple(map(''.join, zip(*roll_rocks(prd))))))


def solve_part_two(prd):
    """
    Solves Part Two: detects repeating grid cycles and fast-forwards to 1,000,000,000 spin cycles.

    :param prd: Initial grid tuple.
    :return: Final load on the north support beam after 1,000,000,000 cycles.
    """
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
