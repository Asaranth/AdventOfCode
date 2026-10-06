"""
Day 13: Point of Incidence

Detects reflection lines of symmetry across pattern grids with exact reflection and single-smudge tolerance.
"""

from utils import get_input_data

data = get_input_data(13).split('\n\n')


def adjust(upper, lower):
    """
    Truncates two list halves to equal length for symmetric comparison.

    :param upper: Reversed top half lines.
    :param lower: Bottom half lines.
    :return: Tuple of equalised (upper, lower) slices.
    """
    length = min(len(upper), len(lower))
    return upper[:length], lower[:length]


def find_mirror(m):
    """
    Finds a horizontal reflection line where upper and lower halves match perfectly.

    :param m: 2D grid matrix of characters.
    :return: Row index of the reflection line (or 0 if none found).
    """
    for r in range(1, len(m)):
        upper, lower = adjust(m[:r][::-1], m[r:])
        if upper == lower:
            return r
    return 0


def find_mirror_with_smudge(m):
    """
    Finds a horizontal reflection line having exactly one character difference (smudge) across reflections.

    :param m: 2D grid matrix of characters.
    :return: Row index of the reflection line (or 0 if none found).
    """
    for r in range(1, len(m)):
        upper, lower = m[:r][::-1], m[r:]
        if sum(sum(0 if a == b else 1 for a, b in zip(x, y)) for x, y in zip(upper, lower)) == 1:
            return r
    return 0


def solve_part_one():
    """
    Solves Part One: computes summary score across all patterns using exact reflection lines.

    :return: Summary score combining vertical and horizontal reflection indices.
    """
    v, h = 0, 0
    for mirror in data:
        m = mirror.splitlines()
        h += find_mirror(m)
        v += find_mirror(list(zip(*m)))
    return v + (h * 100)


def solve_part_two():
    """
    Solves Part Two: computes summary score across all patterns fixing exactly one smudge per grid.

    :return: Summary score combining smudged vertical and horizontal reflection indices.
    """
    v, h = 0, 0
    for mirror in data:
        m = mirror.splitlines()
        h += find_mirror_with_smudge(m)
        v += find_mirror_with_smudge(list(zip(*m)))
    return v + (h * 100)


print('Part One %d' % solve_part_one())
print('Part Two %d' % solve_part_two())
