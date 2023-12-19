data = open('../data/09.txt').read().splitlines()


def calculate_next_sequence(sequence):
    return [sequence[i + 1] - sequence[i] for i in range(len(sequence) - 1)]


def parse_sequence(sequence):
    sequences = [sequence]
    while True:
        curr_sequence = sequences[-1]
        next_sequence = calculate_next_sequence(curr_sequence)
        sequences.append(next_sequence)
        if not any(next_sequence):
            break
    return reversed(sequences)


def extrapolate_future(sequence):
    future = 0
    for i, sequence in enumerate(parse_sequence(sequence)):
        if i == 0:
            continue
        else:
            future += sequence[-1]
    return future


def extrapolate_history(sequence):
    history = 0
    for i, sequence in enumerate(parse_sequence(sequence)):
        if i == 0:
            continue
        else:
            history = sequence[0] - history
    return history


def solve_part_one():
    total = 0
    for line in data:
        sequence = list(map(int, line.split()))
        total += extrapolate_future(sequence)
    return total


def solve_part_two():
    total = 0
    for line in data:
        sequence = list(map(int, line.split()))
        total += extrapolate_history(sequence)
    return total


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
