from re import compile

data = open('data/15.txt').read().replace('\n', '').split(',')
DIVISOR = 256


def get_remainder(dividend):
    quotient = dividend // DIVISOR
    return dividend - quotient * DIVISOR


def run_hash(step):
    value = 0
    for ch in step:
        value += ord(ch)
        value *= 17
        value = get_remainder(value)
    return value


def organise_lenses():
    boxes = [list() for _ in range(256)]
    for part in data:
        if '=' in part:
            label, focal_length = part.split('=')
            i = run_hash(label)
            pattern = compile(rf'^{label}\s\d+$')
            if [l for l in boxes[i] if pattern.match(l)]:
                ii = [l for l, lens in enumerate(boxes[i]) if pattern.match(lens)][0]
                boxes[i][ii] = f'{label} {focal_length}'
            else:
                boxes[i].append(f'{label} {focal_length}')
        if '-' in part:
            label = part.replace('-', '')
            i = run_hash(label)
            boxes[i] = [x for x in boxes[i] if not compile(rf'^{label}\s\d+$').match(x)]
    return boxes


def get_focusing_power(box, slot, focal_length):
    return box * slot * focal_length


def solve_part_one():
    res = 0
    for part in data:
        res += run_hash(part)
    return res


def solve_part_two():
    boxes, res = organise_lenses(), 0
    for b, box in enumerate(boxes, 1):
        for l, lens in enumerate(box, 1):
            res += get_focusing_power(b, l, int(lens.split(' ')[1]))
    return res


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
