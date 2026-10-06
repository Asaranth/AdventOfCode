using System.Text.RegularExpressions;

namespace _2016;

/// <summary>
/// Day 07: Internet Protocol Version 7
/// 
/// Validates IPv7 addresses for TLS and SSL support using ABBA (Autonomous Bridge Bypass Annotation) and ABA/BAB patterns.
/// </summary>
public abstract partial class _07
{
    private static readonly string[] Data;

    static _07() => Data = Task.Run(() => Utils.GetInputData(7)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Checks whether the string segment contains a 4-character palindromic ABBA sequence with distinct inner/outer characters.
    /// </summary>
    /// <param name="segment">String slice to evaluate.</param>
    /// <returns>True if an ABBA pattern is present; otherwise, false.</returns>
    private static bool IsAbba(string segment)
    {
        for (var i = 0; i < segment.Length - 3; i++)
        {
            if (segment[i] == segment[i + 3] && segment[i + 1] == segment[i + 2] && segment[i] != segment[i + 1])
                return true;
        }

        return false;
    }

    /// <summary>
    /// Checks whether the segment contains an ABA sequence and outputs the characters if found.
    /// </summary>
    /// <param name="segment">Three-character or longer slice to evaluate.</param>
    /// <param name="aba">The identified ABA character tuple.</param>
    /// <returns>True if an ABA pattern is present; otherwise, false.</returns>
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

    /// <summary>
    /// Determines whether an IP address supports TLS (contains ABBA in supernet sequences but none in hypernet bracketed sequences).
    /// </summary>
    /// <param name="ip">Raw IPv7 address string.</param>
    /// <returns>True if the IP supports TLS; otherwise, false.</returns>
    private static bool SupportsTls(string ip)
    {
        var hypernets = HypernetRegex().Matches(ip).Select(m => m.Groups[1].Value).ToArray();
        var supernets = SupernetRegex().Split(ip);
        var hasAbbaInHypernet = hypernets.Any(IsAbba);
        var hasAbbaInSupernet = supernets.Any(IsAbba);
        return !hasAbbaInHypernet && hasAbbaInSupernet;
    }

    /// <summary>
    /// Determines whether an IP address supports SSL (contains matching ABA in supernet and BAB in hypernet sequences).
    /// </summary>
    /// <param name="ip">Raw IPv7 address string.</param>
    /// <returns>True if the IP supports SSL; otherwise, false.</returns>
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

    /// <summary>
    /// Solves Part One: counts the number of IP addresses supporting TLS.
    /// </summary>
    /// <returns>Number of TLS-supported IPs.</returns>
    private static int SolvePartOne() => Data.Count(SupportsTls);

    /// <summary>
    /// Solves Part Two: counts the number of IP addresses supporting SSL.
    /// </summary>
    /// <returns>Number of SSL-supported IPs.</returns>
    private static int SolvePartTwo() => Data.Count(SupportsSsl);

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    /// <summary>
    /// Regex pattern matching hypernet sequences enclosed within square brackets.
    /// </summary>
    [GeneratedRegex(@"\[(.*?)\]")]
    private static partial Regex HypernetRegex();

    /// <summary>
    /// Regex pattern for splitting supernet sequences separated by bracketed hypernets.
    /// </summary>
    [GeneratedRegex(@"\[[^\]]+\]")]
    private static partial Regex SupernetRegex();
}