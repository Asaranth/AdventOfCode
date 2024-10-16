from utils import get_input_data

data = get_input_data(13).split('\n\n')


def adjust(upper, lower):
    length = min(len(upper), len(lower))
    return upper[:length], lower[:length]


def find_mirror(m):
    for r in range(1, len(m)):
        upper, lower = adjust(m[:r][::-1], m[r:])
        if upper == lower:
            return r
    return 0


def find_mirror_with_smudge(m):
    for r in range(1, len(m)):
        upper, lower = m[:r][::-1], m[r:]
        if sum(sum(0 if a == b else 1 for a, b, in zip(x, y)) for x, y in zip(upper, lower)) == 1:
            return r
    return 0


def solve_part_one():
    v, h = 0, 0
    for mirror in data:
        m = mirror.splitlines()
        h += find_mirror(m)
        v += find_mirror(list(zip(*m)))
    return v + (h * 100)


def solve_part_two():
    v, h = 0, 0
    for mirror in data:
        m = mirror.splitlines()
        h += find_mirror_with_smudge(m)
        v += find_mirror_with_smudge(list(zip(*m)))
    return v + (h * 100)


print('Part One %d' % solve_part_one())
print('Part Two %d' % solve_part_two())
