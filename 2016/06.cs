namespace _2016;

public abstract class _06
{
    private static readonly string[] Data;

    static _06() => Data = Task.Run(() => Utils.GetInputData(6)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

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

    private static string SolvePartOne()
    {
        var colFreq = GetColumnFrequencies();
        var result = new char[Data[0].Length];
        for (var i = 0; i < Data[0].Length; i++) result[i] = colFreq[i].OrderByDescending(kvp => kvp.Value).First().Key;

        return new string(result);
    }

    private static string SolvePartTwo()
    {
        var colFreq = GetColumnFrequencies();
        var result = new char[Data[0].Length];
        for (var i = 0; i < Data[0].Length; i++) result[i] = colFreq[i].OrderBy(kvp => kvp.Value).First().Key;

        return new string(result);
    }

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}