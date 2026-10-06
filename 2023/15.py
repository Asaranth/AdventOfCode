"""
Day 15: Lens Library

Implements the HASH algorithm and simulates lens arrangement across 256 hashmap boxes.
"""

from re import compile
from utils import get_input_data

data = get_input_data(15).replace('\n', '').split(',')
DIVISOR = 256


def get_remainder(dividend):
    """
    Computes remainder modulo DIVISOR using integer arithmetic.

    :param dividend: Numeric dividend value.
    :return: Remainder of dividend divided by DIVISOR.
    """
    quotient = dividend // DIVISOR
    return dividend - quotient * DIVISOR


def run_hash(step):
    """
    Computes the 8-bit HASH value for a character string.

    :param step: Input string label or operation step.
    :return: Resulting integer hash value (0–255).
    """
    value = 0
    for ch in step:
        value += ord(ch)
        value *= 17
        value = get_remainder(value)
    return value


def organise_lenses():
    """
    Simulates inserting, replacing, and removing labelled lenses in 256 boxes according to sequence steps.

    :return: List of 256 box lists containing lens label and focal length strings.
    """
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
    """
    Computes focusing power for a single lens given its 1-based box number, slot index, and focal length.

    :param box: 1-based box index.
    :param slot: 1-based slot index within the box.
    :param focal_length: Focal length integer of the lens.
    :return: Focusing power value.
    """
    return box * slot * focal_length


def solve_part_one():
    """
    Solves Part One: computes the sum of HASH results for each comma-separated initialisation step.

    :return: Sum of hash values across all steps.
    """
    res = 0
    for part in data:
        res += run_hash(part)
    return res


def solve_part_two():
    """
    Solves Part Two: organises lenses into boxes and computes total combined focusing power.

    :return: Total focusing power across all installed lenses.
    """
    boxes, res = organise_lenses(), 0
    for b, box in enumerate(boxes, 1):
        for l, lens in enumerate(box, 1):
            res += get_focusing_power(b, l, int(lens.split(' ')[1]))
    return res


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
