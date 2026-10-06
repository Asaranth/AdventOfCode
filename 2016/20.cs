namespace _2016;

/// <summary>
/// Day 20: Firewall Rules
/// 
/// Merges blocked 32-bit IP integer ranges to find the lowest valid unblocked IP and count total permitted IPs.
/// </summary>
public static class _20
{
    private static readonly string[] Data;

    static _20() => Data = Task.Run(() => Utils.GetInputData(20)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Parses and sorts blocked IP intervals in ascending order by start address.
    /// </summary>
    /// <returns>Ordered list of blocked interval ranges.</returns>
    private static List<(ulong Start, ulong End)> GetBlockedRanges() =>
        Data.Select(line => line.Split('-'))
            .Select(parts => (Start: ulong.Parse(parts[0]), End: ulong.Parse(parts[1])))
            .OrderBy(range => range.Start)
            .ToList();

    /// <summary>
    /// Solves Part One: finds the lowest-valued non-blocked IP address starting from 0.
    /// </summary>
    /// <returns>The lowest allowed 32-bit IP integer.</returns>
    private static ulong SolvePartOne()
    {
        var blockedRanges = GetBlockedRanges();
        ulong lowestNonBlockedIp = 0;

        foreach (var range in blockedRanges.TakeWhile(range => lowestNonBlockedIp >= range.Start).Where(range => lowestNonBlockedIp <= range.End))
            lowestNonBlockedIp = range.End + 1;

        return lowestNonBlockedIp;
    }

    /// <summary>
    /// Solves Part Two: counts the total number of allowed IP addresses within the full 32-bit address space.
    /// </summary>
    /// <returns>Total number of allowed IP addresses.</returns>
    private static ulong SolvePartTwo()
    {
        var blockedRanges = GetBlockedRanges();
        ulong allowedIpCount = 0;
        ulong currentIp = 0;
        const ulong maxIp = 4294967295UL;

        foreach (var range in blockedRanges)
        {
            if (currentIp < range.Start) allowedIpCount += range.Start - currentIp;
            if (currentIp <= range.End) currentIp = range.End + 1;
        }

        if (currentIp <= maxIp) allowedIpCount += maxIp - currentIp + 1;

        return allowedIpCount;
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