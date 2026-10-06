"""
Day 07: Camel Cards

Ranks Camel Cards hands by hand type classification and card strength ordering, including joker wildcards.
"""

from utils import get_input_data

data = get_input_data(7).splitlines()
letter_map = {'T': 'A', 'J': 'B', 'Q': 'C', 'K': 'D', 'A': 'E'}


def get_hand_type(hand):
    """
    Classifies a five-card hand into numeric strength based on card frequency counts.

    :param hand: Five-character string of card labels.
    :return: Integer hand score from 0 (High Card) to 6 (Five of a Kind).
    """
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
    """
    Generates all possible concrete hands resulting from replacing joker ('J') cards with standard ranks.

    :param hand: Five-character string containing potential jokers.
    :return: List of all expanded hand candidate strings.
    """
    if hand == '':
        return ['']
    return [x + y for x in ('23456789TQKA' if hand[0] == 'J' else hand[0]) for y in play_joker_rule(hand[1:])]


def classify(hand):
    """
    Determines optimal hand type strength, taking joker wildcard rules into account when active.

    :param hand: Five-character card string.
    :return: Best hand type score.
    """
    if letter_map['J'] == '0':
        return max(map(get_hand_type, play_joker_rule(hand)))
    return get_hand_type(hand)


def get_hand_strength(hand):
    """
    Constructs a sorting tuple consisting of hand type rank and mapped card values.

    :param hand: Five-character card string.
    :return: Tuple of (hand_type_score, mapped_card_values).
    """
    return classify(hand), [letter_map.get(card, card) for card in hand]


def solve_part_one():
    """
    Solves Part One: ranks hands with standard Jacks and calculates total winnings.

    :return: Total winnings calculated as sum of rank multiplied by bid.
    """
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
    """
    Solves Part Two: re-evaluates hands with Jokers as individual wildcards and weakest individual card.

    :return: Total winnings under joker rules.
    """
    letter_map['J'] = '0'
    return solve_part_one()


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
