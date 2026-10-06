namespace _2016;

/// <summary>
/// Day 06: Signals and Noise
/// 
/// Reconstructs error-corrected messages from noisy character streams using frequency analysis per column.
/// </summary>
public abstract class _06
{
    private static readonly string[] Data;

    static _06() => Data = Task.Run(() => Utils.GetInputData(6)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Computes character frequency maps for each character column in the input messages.
    /// </summary>
    /// <returns>A dictionary mapping column index to a character-to-count frequency dictionary.</returns>
    private static Dictionary<int, Dictionary<char, int>> GetColumnFrequencies()
    {
        var colFreq = new Dictionary<int, Dictionary<char, int>>();

        for (var i = 0; i < Data[0].Length; i++) colFreq[i] = new Dictionary<char, int>();

        foreach (var line in Data)
            for (var i = 0; i < line.Length; i++)
            {
                if (!colFreq[i].ContainsKey(line[i])) colFreq[i][line[i]] = 0;
                colFreq[i][line[i]]++;
            }

        return colFreq;
    }

    /// <summary>
    /// Solves Part One: constructs the message using the most common character in each column.
    /// </summary>
    /// <returns>The error-corrected message string.</returns>
    private static string SolvePartOne()
    {
        var colFreq = GetColumnFrequencies();
        var result = new char[Data[0].Length];
        for (var i = 0; i < Data[0].Length; i++) result[i] = colFreq[i].OrderByDescending(kvp => kvp.Value).First().Key;

        return new string(result);
    }

    /// <summary>
    /// Solves Part Two: constructs the message using the least common character in each column.
    /// </summary>
    /// <returns>The modified error-corrected message string.</returns>
    private static string SolvePartTwo()
    {
        var colFreq = GetColumnFrequencies();
        var result = new char[Data[0].Length];
        for (var i = 0; i < Data[0].Length; i++) result[i] = colFreq[i].OrderBy(kvp => kvp.Value).First().Key;

        return new string(result);
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