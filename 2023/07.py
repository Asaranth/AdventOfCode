data = open('data/07.txt').read().splitlines()
letter_map = {'T': 'A', 'J': 'B', 'Q': 'C', 'K': 'D', 'A': 'E'}


def get_hand_type(hand):
    counts = [hand.count(card) for card in hand]
    if 5 in counts:
        return 6
    if 4 in counts:
        return 5
    if 3 in counts:
        if 2 in counts:
            return 4
        return 3
    if counts.count(2) == 4:
        return 2
    if 2 in counts:
        return 1
    return 0


def play_joker_rule(hand):
    if hand == '':
        return ['']
    return [x + y for x in ('23456789TQKA' if hand[0] == 'J' else hand[0]) for y in play_joker_rule(hand[1:])]


def classify(hand):
    if letter_map['J'] == '0':
        return max(map(get_hand_type, play_joker_rule(hand)))
    return get_hand_type(hand)


def get_hand_strength(hand):
    return classify(hand), [letter_map.get(card, card) for card in hand]


def solve_part_one():
    hands = []
    total = 0
    for line in data:
        hand, bid = line.split()
        hands.append((hand, int(bid)))
    hands.sort(key = lambda h: get_hand_strength(h[0]))
    for rank, (hand, bid) in enumerate(hands, 1):
        total += rank * bid
    return total


def solve_part_two():
    letter_map['J'] = '0'
    return solve_part_one()


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
