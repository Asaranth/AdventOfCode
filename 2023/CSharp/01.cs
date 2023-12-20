using System.Text.RegularExpressions;

namespace CSharp;

public abstract partial class DayOne
{
    private static IEnumerable<string> _data = Enumerable.Empty<string>();
    private static readonly Dictionary<string, int> NumberMap = new()
    {
        {"one", 1},
        {"two", 2},
        {"three", 3},
        {"four", 4},
        {"five", 5},
        {"six", 6},
        {"seven", 7},
        {"eight", 8},
        {"nine", 9}
    };


    private static void GetData() =>
        _data = File.ReadLines(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\data\01.txt"));

    public static void Run()
    {
        GetData();
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    private static int GetCalibrationValue(IReadOnlyList<int> numbers) => int.Parse($"{numbers[0]}{numbers[^1]}");

    private static int WordsToNumbers(string line)
    {
        var pattern = "(?=(" + string.Join('|', NumberMap.Keys) + "|[0-9]))";
        var result = new List<int>();

        foreach (Match match in Regex.Matches(line, pattern))
            result.Add(int.TryParse(match.Groups[1].Value, out var digit)
                ? digit
                : NumberMap.Where(n => n.Key == match.Groups[1].Value).Select(n => n.Value).First());

        return GetCalibrationValue(result);
    }

    private static int SolvePartOne() =>
        _data.Select(line => GetCalibrationValue(RxDigit().Matches(line).Select(m => int.Parse(m.Value)).ToArray())).Sum();

    private static int SolvePartTwo() => _data.Select(WordsToNumbers).Sum();

    [GeneratedRegex(@"\d")]
    private static partial Regex RxDigit();
}