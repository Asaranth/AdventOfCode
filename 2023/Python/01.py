from re import findall

data = open('../data/01.txt').read().splitlines()
number_map = {'one': 1, 'two': 2, 'three': 3, 'four': 4, 'five': 5, 'six': 6, 'seven': 7, 'eight': 8, 'nine': 9}
pattern = r'(?=(' + '|'.join(list(number_map.keys()) + list(map(str, number_map.values()))) + '))'


def get_calibration_value(numbers):
    if len(numbers) == 1:
        return int('{0}{0}'.format(numbers[0]))
    return int('%s%s' % (numbers[0], numbers[-1]))


def words_to_numbers(line):
    numbers = [number_map.get(match, match) for match in findall(pattern, line)]
    return get_calibration_value(numbers)


def solve_part_one():
    results = []
    for line in data:
        results.append(get_calibration_value(findall(r'\d', line)))
    return sum(results)


def solve_part_two():
    results = []
    for line in data:
        results.append(words_to_numbers(line))
    return sum(results)


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
