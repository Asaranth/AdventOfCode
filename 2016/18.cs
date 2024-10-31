namespace _2016;

public static class _18
{
    private static readonly string Data;

    static _18() => Data = Task.Run(() => Utils.GetInputData(18)).Result.Trim();

    private static int CountSafeTiles(string row) => row.Count(tile => tile == '.');

    private static string GenerateNextRow(string currentRow)
    {
        var nextRow = new char[currentRow.Length];
        for (var i = 0; i < currentRow.Length; i++)
        {
            var left = i > 0 ? currentRow[i - 1] : '.';
            var center = currentRow[i];
            var right = i < currentRow.Length - 1 ? currentRow[i + 1] : '.';

            var isTrap = (left == '^' && center == '^' && right == '.') ||
                         (left == '.' && center == '^' && right == '^') ||
                         (left == '^' && center == '.' && right == '.') ||
                         (left == '.' && center == '.' && right == '^');

            nextRow[i] = isTrap ? '^' : '.';
        }

        return new string(nextRow);
    }

    private static int CountTotalSafeTiles(int totalRows)
    {
        var currentRow = Data;
        var safeTilesCount = 0;
        for (var row = 0; row < totalRows; row++)
        {
            safeTilesCount += CountSafeTiles(currentRow);
            currentRow = GenerateNextRow(currentRow);
        }

        return safeTilesCount;
    }

    private static int SolvePartOne() => CountTotalSafeTiles(40);

    private static int SolvePartTwo() => CountTotalSafeTiles(400000);

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}