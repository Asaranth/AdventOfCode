"""
Day 02: Cube Conundrum

Evaluates cube reveal games against colour count limits and calculates minimum set power.
"""

from re import search
from utils import get_input_data

data = get_input_data(2).splitlines()


class Round:
    """
    Represents a single round of revealed coloured cubes.
    """
    RED_LIMIT = 12
    GREEN_LIMIT = 13
    BLUE_LIMIT = 14

    def __init__(self, color_counts_str):
        """
        Initialises a round from a comma-separated cube count string.

        :param color_counts_str: Comma-separated string of counts and colours (e.g. '3 blue, 4 red').
        """
        self.red = 0
        self.green = 0
        self.blue = 0
        self.parse_round(color_counts_str)

    def parse_round(self, color_counts_str):
        """
        Parses colour counts into red, green, and blue properties.

        :param color_counts_str: Comma-separated string of counts and colours.
        """
        parts = color_counts_str.split(', ')
        for part in parts:
            count, color = part.split(' ')
            if color == 'red':
                self.red = int(count)
            elif color == 'green':
                self.green = int(count)
            elif color == 'blue':
                self.blue = int(count)

    def is_possible(self):
        """
        Checks whether all cube counts in this round satisfy the maximum allowable limits.

        :return: True if the round does not exceed any colour limits; otherwise, False.
        """
        return self.red <= Round.RED_LIMIT and self.green <= Round.GREEN_LIMIT and self.blue <= Round.BLUE_LIMIT


class Game:
    """
    Represents a full game comprising multiple cube reveal rounds.
    """

    def __init__(self, game_str):
        """
        Initialises a game record with an ID and list of rounds.

        :param game_str: Full game record line from puzzle input.
        """
        self.id = int(search(r'\d+', game_str).group())
        self.rounds = self.parse_rounds(game_str)

    @staticmethod
    def parse_rounds(game_str):
        """
        Parses all semicolon-separated rounds in a game line.

        :param game_str: Full game line string.
        :return: List of Round instances.
        """
        return [Round(game_part) for game_part in game_str[game_str.find(':') + 2:].split('; ')]

    def is_possible(self):
        """
        Determines whether every round in the game satisfies the colour limits.

        :return: True if all rounds are possible; otherwise, False.
        """
        for r in self.rounds:
            if not r.is_possible():
                return False
        return True

    def game_power(self):
        """
        Calculates the power of the minimum set of cubes needed to make the game possible.

        :return: Product of the maximum red, green, and blue cube counts observed across rounds.
        """
        min_red = 0
        min_green = 0
        min_blue = 0
        for r in self.rounds:
            if r.red > min_red:
                min_red = r.red
            if r.green > min_green:
                min_green = r.green
            if r.blue > min_blue:
                min_blue = r.blue
        return min_red * min_green * min_blue


games = []
for line in data:
    games.append(Game(line))


def solve_part_one():
    """
    Solves Part One: sums the IDs of all possible games satisfying the configuration limits.

    :return: Sum of possible game IDs.
    """
    return sum(list(map(lambda game: game.id, filter(lambda game: game.is_possible(), games))))


def solve_part_two():
    """
    Solves Part Two: sums the power of the minimum cube sets across all games.

    :return: Sum of powers for all games.
    """
    return sum(list(map(lambda game: game.game_power(), games)))


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
