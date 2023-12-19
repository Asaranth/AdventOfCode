from math import gcd

instructions, _, *mapping = open('../data/08.txt').read().splitlines()
direction_map = {}

for line in mapping:
    pos, targets = line.split(' = ')
    direction_map[pos] = targets[1:-1].split(', ')


def solve_part_one(directions):
    steps = 0
    current_pos = 'AAA'
    while current_pos != 'ZZZ':
        steps += 1
        current_pos = direction_map[current_pos][0 if directions[0] == 'L' else 1]
        directions = directions[1:] + directions[0]
    return steps


def solve_part_two(directions):
    positions = [key for key in direction_map if key.endswith('A')]
    cycles = []
    for current_pos in positions:
        cycle = []
        step_count = 0
        first_z = None
        while True:
            while step_count == 0 or not current_pos.endswith('Z'):
                step_count += 1
                current_pos = direction_map[current_pos][0 if directions[0] == 'L' else 1]
                directions = directions[1:] + directions[0]
            cycle.append(step_count)
            if first_z is None:
                first_z = current_pos
                step_count = 0
            elif current_pos == first_z:
                break
        cycles.append(cycle)
    nums = [cycle[0] for cycle in cycles]
    lcm = nums.pop()
    for num in nums:
        lcm = lcm * num // gcd(lcm, num)
    return lcm


print('Part One: %d' % solve_part_one(instructions))
print('Part Two: %d' % solve_part_two(instructions))
