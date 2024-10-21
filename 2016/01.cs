namespace _2016;

public abstract class _01
{
    private static readonly string[] Data;

    static _01() => Data = Task.Run(() => Utils.GetInputData(1)).Result.Split(", ");

    private static (char turn, int distance) ParseInstruction(string instruction) =>
        (instruction[0], int.Parse(instruction[1..]));

    private static char UpdateDirection(char currentDirection, char turn) => currentDirection switch
    {
        'N' => turn == 'R' ? 'E' : 'W',
        'E' => turn == 'R' ? 'S' : 'N',
        'S' => turn == 'R' ? 'W' : 'E',
        'W' => turn == 'R' ? 'N' : 'S',
        _ => currentDirection
    };

    private static (int X, int Y) Move(int x, int y, char direction, int distance) => direction switch
    {
        'N' => (x, y + distance),
        'E' => (x + distance, y),
        'S' => (x, y - distance),
        'W' => (x - distance, y),
        _ => (x, y)
    };

    private static int SolvePartOne()
    {
        int x = 0, y = 0;
        var direction = 'N';

        foreach (var instruction in Data)
        {
            var (turn, distance) = ParseInstruction(instruction);
            direction = UpdateDirection(direction, turn);
            (x, y) = Move(x, y, direction, distance);
        }

        return Math.Abs(x) + Math.Abs(y);
    }

    private static int SolvePartTwo()
    {
        int x = 0, y = 0;
        var direction = 'N';
        var visitedLocations = new HashSet<(int X, int Y)> { (x, y) };

        foreach (var instruction in Data)
        {
            var (turn, distance) = ParseInstruction(instruction);
            direction = UpdateDirection(direction, turn);

            for (var i = 0; i < distance; i++)
            {
                (x, y) = Move(x, y, direction, 1);
                if (!visitedLocations.Add((x, y))) return Math.Abs(x) + Math.Abs(y);
            }
        }

        throw new InvalidOperationException("Failed to find a repeated location.");
    }

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}