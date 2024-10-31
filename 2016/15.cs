namespace _2016;

public abstract class _15
{
    private static readonly string[] Data;

    static _15() => Data = Task.Run(() => Utils.GetInputData(15)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    private class Disc(int positions, int initialPosition)
    {
        public int Positions { get; } = positions;
        public int InitialPosition { get; } = initialPosition;
    }

    private static List<Disc> ParseDiscs() => Data.Select(line =>
    {
        var parts = line.Split([' ', '#', ';', '.'], StringSplitOptions.RemoveEmptyEntries);
        var positions = int.Parse(parts[3]);
        var initialPosition = int.Parse(parts.Last());
        return new Disc(positions, initialPosition);
    }).ToList();

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

    private static int SolvePartOne() => FindFirstSuccessfulDropTime(ParseDiscs());

    private static int SolvePartTwo()
    {
        var discs = ParseDiscs();
        discs.Add(new Disc(11, 0));
        return FindFirstSuccessfulDropTime(discs);
    }

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}