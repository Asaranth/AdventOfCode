namespace _2016;

/// <summary>
/// Day 01: No Time for a Taxicab
/// 
/// Traces grid navigation instructions using coordinate translation, 90-degree turns, and intersection tracking.
/// </summary>
public abstract class _01
{
    private static readonly string[] Data;

    static _01() => Data = Task.Run(() => Utils.GetInputData(1)).Result.Split(", ");

    /// <summary>
    /// Parses a single instruction string into turn direction and distance.
    /// </summary>
    /// <param name="instruction">Instruction token (e.g. "R2", "L3").</param>
    /// <returns>A tuple containing the turn direction ('L' or 'R') and integer step distance.</returns>
    private static (char turn, int distance) ParseInstruction(string instruction) =>
        (instruction[0], int.Parse(instruction[1..]));

    /// <summary>
    /// Computes the new facing direction after a left or right turn.
    /// </summary>
    /// <param name="currentDirection">Current cardinal heading ('N', 'E', 'S', 'W').</param>
    /// <param name="turn">Turn direction ('L' or 'R').</param>
    /// <returns>The resulting cardinal direction character.</returns>
    private static char UpdateDirection(char currentDirection, char turn) => currentDirection switch
    {
        'N' => turn == 'R' ? 'E' : 'W',
        'E' => turn == 'R' ? 'S' : 'N',
        'S' => turn == 'R' ? 'W' : 'E',
        'W' => turn == 'R' ? 'N' : 'S',
        _ => currentDirection
    };

    /// <summary>
    /// Translates 2D coordinates in the specified cardinal direction by distance.
    /// </summary>
    /// <param name="x">Current X coordinate.</param>
    /// <param name="y">Current Y coordinate.</param>
    /// <param name="direction">Cardinal direction to translate along.</param>
    /// <param name="distance">Distance in grid units.</param>
    /// <returns>The updated (X, Y) coordinate tuple.</returns>
    private static (int X, int Y) Move(int x, int y, char direction, int distance) => direction switch
    {
        'N' => (x, y + distance),
        'E' => (x + distance, y),
        'S' => (x, y - distance),
        'W' => (x - distance, y),
        _ => (x, y)
    };

    /// <summary>
    /// Solves Part One: computes Manhattan distance from the starting position to the final destination.
    /// </summary>
    /// <returns>The Manhattan distance for Part One.</returns>
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

    /// <summary>
    /// Solves Part Two: finds the Manhattan distance to the first location visited twice.
    /// </summary>
    /// <returns>The Manhattan distance to the first repeated coordinate.</returns>
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

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}