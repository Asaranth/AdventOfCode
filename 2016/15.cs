namespace _2016;

/// <summary>
/// Day 15: Timing is Everything
/// 
/// Calculates capsule drop timing through rotating discs using modular arithmetic.
/// </summary>
public abstract class _15
{
    private static readonly string[] Data;

    static _15() => Data = Task.Run(() => Utils.GetInputData(15)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Represents a rotating kinetic sculpture disc with a specific number of positions and starting index.
    /// </summary>
    private class Disc(int positions, int initialPosition)
    {
        /// <summary>
        /// Total number of rotational positions on the disc.
        /// </summary>
        public int Positions { get; } = positions;

        /// <summary>
        /// Position index at time t=0.
        /// </summary>
        public int InitialPosition { get; } = initialPosition;
    }

    /// <summary>
    /// Parses disc configuration specifications from puzzle input.
    /// </summary>
    /// <returns>List of parsed Disc objects.</returns>
    private static List<Disc> ParseDiscs() => Data.Select(line =>
    {
        var parts = line.Split([' ', '#', ';', '.'], StringSplitOptions.RemoveEmptyEntries);
        var positions = int.Parse(parts[3]);
        var initialPosition = int.Parse(parts.Last());
        return new Disc(positions, initialPosition);
    }).ToList();

    /// <summary>
    /// Checks whether dropping a capsule at a given time passes through all disc slots at position 0.
    /// </summary>
    /// <param name="discs">Ordered list of discs from top to bottom.</param>
    /// <param name="time">Time of drop in seconds.</param>
    /// <returns>True if the capsule falls through every disc unobstructed; otherwise, false.</returns>
    private static bool IsSuccessfulDrop(List<Disc> discs, int time)
    {
        for (var i = discs.Count - 1; i >= 0; i--)
        {
            var disc = discs[i];
            var discPositionAtTime = (disc.InitialPosition + time + i + 1) % disc.Positions;
            if (discPositionAtTime != 0) return false;
        }
        return true;
    }

    /// <summary>
    /// Finds the first non-negative time at which a dropped capsule successfully passes through all discs.
    /// </summary>
    /// <param name="discs">Collection of discs.</param>
    /// <returns>The earliest successful drop time.</returns>
    private static int FindFirstSuccessfulDropTime(IEnumerable<Disc> discs)
    {
        var time = 0;
        var discsList = discs.ToList();

        while (true)
        {
            if (IsSuccessfulDrop(discsList, time)) return time;
            time++;
        }
    }

    /// <summary>
    /// Solves Part One: finds the earliest time to press the button for the initial set of discs.
    /// </summary>
    /// <returns>First successful drop time for Part One.</returns>
    private static int SolvePartOne() => FindFirstSuccessfulDropTime(ParseDiscs());

    /// <summary>
    /// Solves Part Two: finds the earliest time when an additional 11-position disc is added at the bottom.
    /// </summary>
    /// <returns>First successful drop time for Part Two.</returns>
    private static int SolvePartTwo()
    {
        var discs = ParseDiscs();
        discs.Add(new Disc(11, 0));
        return FindFirstSuccessfulDropTime(discs);
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