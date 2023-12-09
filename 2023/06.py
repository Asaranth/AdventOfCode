times, distances = [list(map(int, line.split(':')[1].split())) for line in open('data/06.txt')]


def get_win_conditions(time, distance):
    win_conditions = 0
    for hold in range(time):
        if hold * (time - hold) > distance:
            win_conditions += 1
    return win_conditions


def solve_part_one():
    total_win_conditions = 1
    for time, distance in zip(times, distances):
        total_win_conditions *= get_win_conditions(time, distance)
    return total_win_conditions


def solve_part_two():
    time = int(''.join(map(str, times)))
    distance = int(''.join(map(str, distances)))
    return get_win_conditions(time, distance)


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
