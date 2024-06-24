from collections import deque
from math import lcm

data = open('data/20.txt').read().splitlines()


class Module:
    def __init__(self, name, t, outputs):
        self.name = name
        self.type = t
        self.outputs = outputs
        if t == '%':
            self.memory = 'off'
        else:
            self.memory = {}


def setup_data():
    modules = {}
    broadcast_targets = []
    for line in data:
        left, right = line.strip().split(' -> ')
        o = right.split(', ')
        if left == 'broadcaster':
            broadcast_targets = o
        else:
            n = left[1:]
            modules[n] = Module(n, left[0], o)
    for n, m in modules.items():
        for output in m.outputs:
            if output in modules and modules[output].type == '&':
                modules[output].memory[n] = 'low'
    return modules, broadcast_targets


def press_button(queue, module, origin, pulse):
    if module.type == '%':
        if pulse == 'low':
            module.memory = 'on' if module.memory == 'off' else 'off'
            outgoing = 'high' if module.memory == 'on' else 'low'
            for x in module.outputs:
                queue.append((module.name, x, outgoing))
    else:
        module.memory[origin] = pulse
        outgoing = 'low' if all(x == 'high' for x in module.memory.values()) else 'high'
        for x in module.outputs:
            queue.append((module.name, x, outgoing))


def solve_part_one():
    modules, broadcast_targets = setup_data()
    low = high = 0
    for _ in range(1000):
        low += 1
        queue = deque([('broadcaster', bt, 'low') for bt in broadcast_targets])
        while queue:
            origin, target, pulse = queue.popleft()
            if pulse == 'low':
                low += 1
            else:
                high += 1
            if target not in modules:
                continue
            press_button(queue, modules[target], origin, pulse)
    return low * high


def solve_part_two():
    modules, broadcast_targets = setup_data()
    (feed,) = [name for name, module in modules.items() if 'rx' in module.outputs]
    cycle_lengths = {}
    seen = {name: 0 for name, module in modules.items() if feed in module.outputs}
    presses = 0
    while True:
        presses += 1
        queue = deque([('broadcaster', bt, 'low') for bt in broadcast_targets])
        while queue:
            origin, target, pulse = queue.popleft()
            if target not in modules:
                continue
            module = modules[target]
            if module.name == feed and pulse == 'high':
                seen[origin] += 1
                if origin not in cycle_lengths:
                    cycle_lengths[origin] = presses
                else:
                    assert presses == seen[origin] * cycle_lengths[origin]
                if all(seen.values()):
                    x = 1
                    for cycle_lengths in cycle_lengths.values():
                        x = lcm(x, cycle_lengths)
                    return x
            press_button(queue, module, origin, pulse)


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
