inputs, *blocks = open('../data/05.txt').read().split('\n\n')
inputs = list(map(int, inputs.split(':')[1].split()))


def solve_part_one(seeds):
    for block in blocks:
        ranges = []
        for line in block.splitlines()[1:]:
            ranges.append(list(map(int, line.split())))
        new = []
        for seed in seeds:
            for destination, source, range_length in ranges:
                if seed in range(source, source + range_length):
                    new.append(seed - source + destination)
                    break
            else:
                new.append(seed)
        seeds = new
    return min(seeds)


def solve_part_two(seed_ranges):
    seeds = []
    for i in range(0, len(seed_ranges), 2):
        seeds.append((seed_ranges[i], seed_ranges[i] + seed_ranges[i + 1]))
    for block in blocks:
        ranges = []
        for line in block.splitlines()[1:]:
            ranges.append(list(map(int, line.split())))
        new = []
        while seeds:
            start, end = seeds.pop()
            for destination, source, range_length in ranges:
                overlap_start = max(start, source)
                overlap_end = min(end, source + range_length)
                if overlap_start < overlap_end:
                    new.append((overlap_start - source + destination, overlap_end - source + destination))
                    if overlap_start > start:
                        seeds.append((start, overlap_start))
                    if end > overlap_end:
                        seeds.append((overlap_end, end))
                    break
            else:
                new.append((start, end))
        seeds = new
    return min(seeds)[0]


print('Part One: %d' % solve_part_one(inputs))
print('Part Two: %d' % solve_part_two(inputs))
