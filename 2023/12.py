from utils import get_input_data

data = get_input_data(12).splitlines()
data = [line.split() for line in data]
memo = {}


def count_arrangements(config, damaged_map):
    if config == '':
        return 1 if damaged_map == () else 0
    if damaged_map == ():
        return 0 if '#' in config else 1

    key = (config, damaged_map)
    if key in memo:
        return memo[key]

    result = 0
    if config[0] in '.?':
        result += count_arrangements(config[1:], damaged_map)
    if config[0] in '#?':
        if enough_left(config, damaged_map[0]) and all_broken(config, damaged_map[0]) and next_works(config,
                                                                                                     damaged_map[0]):
            result += count_arrangements(config[damaged_map[0] + 1:], damaged_map[1:])

    memo[key] = result
    return result


def enough_left(config, damaged_index):
    return damaged_index <= len(config)


def all_broken(config, damaged_index):
    return '.' not in config[:damaged_index]


def next_works(config, damaged_index):
    return damaged_index == len(config) or config[damaged_index] != '#'


def unfold(config, damaged_map):
    return '?'.join([config] * 5), damaged_map * 5


def solve_part_one():
    total = 0
    for d in data:
        total += count_arrangements(d[0], tuple(map(int, d[1].split(','))))
    return total


def solve_part_two():
    total = 0
    for d in data:
        config, damaged_map = unfold(d[0], tuple(map(int, d[1].split(','))))
        total += count_arrangements(config, damaged_map)
    return total


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
