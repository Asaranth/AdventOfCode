using System.Text.RegularExpressions;

namespace _2016;

public abstract partial class _07
{
    private static readonly string[] Data;

    static _07() => Data = Task.Run(() => Utils.GetInputData(7)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    private static bool IsAbba(string segment)
    {
        for (var i = 0; i < segment.Length - 3; i++)
        {
            if (segment[i] == segment[i + 3] && segment[i + 1] == segment[i + 2] && segment[i] != segment[i + 1])
                return true;
        }

        return false;
    }

    private static bool IsAba(string segment, out (char X, char Y) aba)
    {
        aba = default;
        for (var i = 0; i < segment.Length - 2; i++)
        {
            if (segment[i] != segment[i + 2] || segment[i] == segment[i + 1]) continue;
            aba = (segment[i], segment[i + 1]);
            return true;
        }

        return false;
    }

    private static bool SupportsTls(string ip)
    {
        var hypernets = HypernetRegex().Matches(ip).Select(m => m.Groups[1].Value).ToArray();
        var supernets = SupernetRegex().Split(ip);
        var hasAbbaInHypernet = hypernets.Any(IsAbba);
        var hasAbbaInSupernet = supernets.Any(IsAbba);
        return !hasAbbaInHypernet && hasAbbaInSupernet;
    }

    private static bool SupportsSsl(string ip)
    {
        var hypernets = HypernetRegex().Matches(ip).Select(m => m.Groups[1].Value).ToArray();
        var supernets = SupernetRegex().Split(ip);
        var abas = supernets.SelectMany(supernet =>
            Enumerable.Range(0, supernet.Length - 2)
                .Select(i => (Found: IsAba(supernet.Substring(i, 3), out var aba), Aba: aba))
                .Where(t => t.Found)
                .Select(t => t.Aba)
        ).ToArray();

        return abas.Any(aba => hypernets.Any(hypernet => hypernet.Contains($"{aba.Y}{aba.X}{aba.Y}")));
    }

    private static int SolvePartOne() => Data.Count(SupportsTls);

    private static int SolvePartTwo() => Data.Count(SupportsSsl);

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    [GeneratedRegex(@"\[(.*?)\]")]
    private static partial Regex HypernetRegex();

    [GeneratedRegex(@"\[[^\]]+\]")]
    private static partial Regex SupernetRegex();
}