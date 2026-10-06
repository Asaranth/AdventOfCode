"""
Day 09: Mirage Maintenance

Computes successive sequence differences to perform forward and backward polynomial extrapolation.
"""

from utils import get_input_data

data = get_input_data(9).splitlines()


def calculate_next_sequence(sequence):
    """
    Computes the first-order differences between consecutive elements in a sequence.

    :param sequence: List of integer values.
    :return: List of differences between adjacent elements.
    """
    return [sequence[i + 1] - sequence[i] for i in range(len(sequence) - 1)]


def parse_sequence(sequence):
    """
    Repeatedly calculates differences until a sequence of all zeroes is reached, returning the layers in reverse.

    :param sequence: Initial list of integer values.
    :return: Iterator over difference sequences from all-zeroes layer back to original sequence.
    """
    sequences = [sequence]
    while True:
        curr_sequence = sequences[-1]
        next_sequence = calculate_next_sequence(curr_sequence)
        sequences.append(next_sequence)
        if not any(next_sequence):
            break
    return reversed(sequences)


def extrapolate_future(sequence):
    """
    Extrapolates the next value in the sequence by summing the trailing elements of difference layers.

    :param sequence: Initial list of integer values.
    :return: Extrapolated future integer value.
    """
    future = 0
    for i, sequence in enumerate(parse_sequence(sequence)):
        if i == 0:
            continue
        else:
            future += sequence[-1]
    return future


def extrapolate_history(sequence):
    """
    Extrapolates the preceding value in the sequence by cascading differences from the leading elements.

    :param sequence: Initial list of integer values.
    :return: Extrapolated historical integer value before the first element.
    """
    history = 0
    for i, sequence in enumerate(parse_sequence(sequence)):
        if i == 0:
            continue
        else:
            history = sequence[0] - history
    return history


def solve_part_one():
    """
    Solves Part One: sums all forward-extrapolated future values across input sequences.

    :return: Sum of predicted next values.
    """
    total = 0
    for line in data:
        sequence = list(map(int, line.split()))
        total += extrapolate_future(sequence)
    return total


def solve_part_two():
    """
    Solves Part Two: sums all backward-extrapolated historical values across input sequences.

    :return: Sum of predicted previous values.
    """
    total = 0
    for line in data:
        sequence = list(map(int, line.split()))
        total += extrapolate_history(sequence)
    return total


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
