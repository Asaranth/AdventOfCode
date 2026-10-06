namespace _2016;

/// <summary>
/// Day 18: Like a Rogue
/// 
/// Simulates cellular automata trap rows using 3-tile neighbourhood rules.
/// </summary>
public static class _18
{
    private static readonly string Data;

    static _18() => Data = Task.Run(() => Utils.GetInputData(18)).Result.Trim();

    /// <summary>
    /// Counts safe tiles ('.') in a row string.
    /// </summary>
    /// <param name="row">Row character string.</param>
    /// <returns>Number of safe tiles.</returns>
    private static int CountSafeTiles(string row) => row.Count(tile => tile == '.');

    /// <summary>
    /// Computes the subsequent row of floor tiles based on the 3-tile neighbourhood of the previous row.
    /// </summary>
    /// <param name="currentRow">Previous row string.</param>
    /// <returns>Next row string with trap ('^') and safe ('.') tiles.</returns>
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

    /// <summary>
    /// Generates rows up to totalRows and sums all safe tiles encountered.
    /// </summary>
    /// <param name="totalRows">Total row count to simulate.</param>
    /// <returns>Cumulative count of safe tiles.</returns>
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

    /// <summary>
    /// Solves Part One: counts safe tiles across 40 rows.
    /// </summary>
    /// <returns>Safe tile count for 40 rows.</returns>
    private static int SolvePartOne() => CountTotalSafeTiles(40);

    /// <summary>
    /// Solves Part Two: counts safe tiles across 400,000 rows.
    /// </summary>
    /// <returns>Safe tile count for 400,000 rows.</returns>
    private static int SolvePartTwo() => CountTotalSafeTiles(400000);

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}