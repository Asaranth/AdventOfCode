from re import findall
from utils import get_input_data

wfs, ratings = get_input_data(19).split('\n\n')
workflows = {}
operators = {
    '>': int.__gt__,
    '<': int.__lt__
}


def build_workflows():
    for workflow in wfs.splitlines():
        key, rules = workflow[:-1].split('{')
        rules = rules.split(',')
        workflows[key] = ([], rules.pop())
        for rule in rules:
            comparison, target = rule.split(':')
            workflows[key][0].append((comparison[0], comparison[1], int(comparison[2:]), target))


def run_workflow(xmas, key = 'in'):
    if key == 'R':
        return False
    if key == 'A':
        return True
    rules, fallback = workflows[key]
    for key, operator, n, target in rules:
        if operators[operator](xmas[key], n):
            return run_workflow(xmas, target)
    return run_workflow(xmas, fallback)


def count_possible_combinations(ranges, key = 'in'):
    if key == 'R':
        return 0
    if key == 'A':
        product = 1
        for low, high in ranges.values():
            product *= high - low + 1
        return product
    rules, fallback = workflows[key]
    total = 0
    for key, operator, n, target in rules:
        low, high = ranges[key]
        if operator == '<':
            true = (low, n - 1)
            false = (n, high)
        else:
            true = (n + 1, high)
            false = (low, n)
        if true[0] <= true[1]:
            copy = dict(ranges)
            copy[key] = true
            total += count_possible_combinations(copy, target)
        if false[0] <= false[1]:
            ranges = dict(ranges)
            ranges[key] = false
        else:
            break
    else:
        total += count_possible_combinations(ranges, fallback)
    return total


def solve_part_one():
    result = 0
    for rating in ratings.splitlines():
        xmas = dict(zip(['x', 'm', 'a', 's'], map(int, findall(r'\d+', rating))))
        if run_workflow(xmas):
            result += sum(xmas.values())
    return result


def solve_part_two():
    return count_possible_combinations({key: (1, 4000) for key in 'xmas'})


build_workflows()
print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
