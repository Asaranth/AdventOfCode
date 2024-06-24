from sympy import symbols, solve

hailstones = [tuple(map(int, line.replace('@', ',').split(','))) for line in open('data/24.txt')]


def solve_part_one():
    total = 0
    for i, hs1 in enumerate(hailstones):
        for hs2 in hailstones[:i]:
            px, py = symbols('px py')
            answers = solve([vy * (px - sx) - vx * (py - sy) for sx, sy, _, vx, vy, _ in [hs1, hs2]])
            if not answers:
                continue
            x, y = answers[px], answers[py]
            if 200000000000000 <= x <= 400000000000000 and 200000000000000 <= y <= 400000000000000:
                if all((x - sx) * vx >= 0 and (y - sy) * vy >= 0 for sx, sy, _, vx, vy, _ in [hs1, hs2]):
                    total += 1
    return total


def solve_part_two():
    xr, yr, zr, vxr, vyr, vzr = symbols('xr, yr, zr, vxr, vyr, vzr')
    equations = []
    for i, (sx, sy, sz, vx, vy, vz) in enumerate(hailstones):
        equations.append((xr - sx) * (vy - vyr) - (yr - sy) * (vx - vxr))
        equations.append((yr - sy) * (vz - vzr) - (zr - sz) * (vy - vyr))
        if i < 2:
            continue
        answers = [solution for solution in solve(equations) if all(x % 1 == 0 for x in solution.values())]
        if len(answers) == 1:
            break
    answer = answers[0]
    return answer[xr] + answer[yr] + answer[zr]


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
