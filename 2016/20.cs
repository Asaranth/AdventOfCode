namespace _2016;

public static class _20
{
    private static readonly string[] Data;

    static _20() => Data = Task.Run(() => Utils.GetInputData(20)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    private static List<(ulong Start, ulong End)> GetBlockedRanges() =>
        Data.Select(line => line.Split('-'))
            .Select(parts => (Start: ulong.Parse(parts[0]), End: ulong.Parse(parts[1])))
            .OrderBy(range => range.Start)
            .ToList();

    private static ulong SolvePartOne()
    {
        var blockedRanges = GetBlockedRanges();
        ulong lowestNonBlockedIp = 0;

        foreach (var range in blockedRanges.TakeWhile(range => lowestNonBlockedIp >= range.Start).Where(range => lowestNonBlockedIp <= range.End))
            lowestNonBlockedIp = range.End + 1;

        return lowestNonBlockedIp;
    }

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

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}