"""
Day 20: Pulse Propagation

Simulates stateful pulse communication circuits and calculates button press cycle synchronisation using LCM.
"""

from collections import deque
from math import lcm
from utils import get_input_data

data = get_input_data(20).splitlines()


class Module:
    """
    Represents a logic communication module (flip-flop, conjunction, or broadcaster).
    """

    def __init__(self, name, t, outputs):
        """
        Initialises module state, type identifier, and output destination names.

        :param name: Module label name.
        :param t: Module type character ('%' for flip-flop, '&' for conjunction).
        :param outputs: List of target module names.
        """
        self.name = name
        self.type = t
        self.outputs = outputs
        if t == '%':
            self.memory = 'off'
        else:
            self.memory = {}


def setup_data():
    """
    Parses configuration lines into module instances and initialises conjunction memory inputs.

    :return: Tuple of (module_dict, broadcast_target_list).
    """
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
    """
    Updates module internal state in response to an incoming pulse and appends outgoing pulses to the queue.

    :param queue: Deque of pending pulse signals (origin, target, pulse_type).
    :param module: Target Module instance receiving the pulse.
    :param origin: Name of the sender module.
    :param pulse: Pulse polarity ('low' or 'high').
    """
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
    """
    Solves Part One: simulates 1000 button presses and computes the product of total low and high pulses transmitted.

    :return: Product of low pulse count and high pulse count.
    """
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
    """
    Solves Part Two: tracks high-pulse cycle periodicities feeding into the penultimate conjunction for 'rx'.

    :return: Fewest button presses required to deliver a low pulse to 'rx'.
    """
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
