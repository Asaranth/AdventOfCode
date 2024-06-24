from re import search

data = open('../data/02.txt').read().splitlines()


class Round:
    RED_LIMIT = 12
    GREEN_LIMIT = 13
    BLUE_LIMIT = 14

    def __init__(self, color_counts_str):
        self.red = 0
        self.green = 0
        self.blue = 0
        self.parse_round(color_counts_str)

    def parse_round(self, color_counts_str):
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
        return self.red <= Round.RED_LIMIT and self.green <= Round.GREEN_LIMIT and self.blue <= Round.BLUE_LIMIT


class Game:
    def __init__(self, game_str):
        self.id = int(search(r'\d+', game_str).group())
        self.rounds = self.parse_rounds(game_str)

    @staticmethod
    def parse_rounds(game_str):
        return [Round(game_part) for game_part in game_str[game_str.find(':') + 2:].split('; ')]

    def is_possible(self):
        for r in self.rounds:
            if not r.is_possible():
                return False
        return True

    def game_power(self):
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
    return sum(list(map(lambda game: game.id, filter(lambda game: game.is_possible(), games))))


def solve_part_two():
    return sum(list(map(lambda game: game.game_power(), games)))


print('Part One: %d' % solve_part_one())
print('Part Two: %d' % solve_part_two())
