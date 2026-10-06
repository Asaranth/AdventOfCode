"""
Day 04: Scratchcards

Calculates scratchcard scores via set intersection and propagates card copies using memoisation.
"""

from utils import get_input_data

data = get_input_data(4).splitlines()
memo = {}


def get_number_sets(card):
    """
    Parses a scratchcard into sets of winning numbers and revealed numbers.

    :param card: Raw scratchcard string.
    :return: List containing winning numbers set and scratched numbers set.
    """
    nums_as_str = card[card.find(':') + 2:].split(' | ')
    return [set(map(int, num_str.split())) for num_str in nums_as_str]


def count_winning_numbers(card):
    """
    Counts how many revealed numbers appear in the winning numbers set for a card.

    :param card: Raw scratchcard string.
    :return: Number of winning matches.
    """
    winning_nums, scratched_nums = get_number_sets(card)
    return len(scratched_nums.intersection(winning_nums))


def get_total_cards_won(card_index):
    """
    Recursively determines the total number of cascading copy cards won from a given card index.

    :param card_index: 0-based index of the scratchcard in the card list.
    :return: Total number of additional scratchcards won.
    """
    if card_index in memo:
        return memo[card_index]
    cards_won = count_winning_numbers(data[card_index])
    total_cards_won = cards_won
    i = 0
    while cards_won > i:
        i += 1
        total_cards_won += get_total_cards_won(card_index + i)
    memo[card_index] = total_cards_won
    return total_cards_won


def solve_part_one():
    """
    Solves Part One: computes total score across all cards with exponential doubling for multiple matches.

    :return: Total points scored.
    """
    points = 0
    for card in data:
        card_score = 0
        count = count_winning_numbers(card)
        if count > 0:
            card_score += 1
            i = 1
            while count > i:
                i += 1
                card_score *= 2
        points += card_score
    return points


def solve_part_two():
    """
    Solves Part Two: calculates the total count of scratchcards processed including all won copies.

    :return: Total number of scratchcards.
    """
    cards = len(data)
    for i, _ in enumerate(data):
        cards += get_total_cards_won(i)
    return cards


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
