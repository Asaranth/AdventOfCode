using System.Text.RegularExpressions;

namespace _2016;

public abstract partial class _04
{
    private static readonly string[] Data;

    static _04() => Data = Task.Run(() => Utils.GetInputData(4)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    private static string DecryptName(string encryptedName, int sectorId) =>
        string.Concat(encryptedName.Select(ch => ch == '-' ? ' ' : (char)('a' + (ch - 'a' + sectorId) % 26)));

    private static int SolvePartOne()
    {
        var sectorIdSum = 0;

        foreach (var room in Data)
        {
            var match = SplitDetails().Match(room);
            if (!match.Success) continue;

            var encryptedName = match.Groups[1].Value;
            var sectorId = int.Parse(match.Groups[2].Value);
            var givenChecksum = match.Groups[3].Value;
            var letterCounts = new Dictionary<char, int>();

            foreach (var ch in encryptedName.Replace("-", ""))
            {
                letterCounts.TryAdd(ch, 0);
                letterCounts[ch]++;
            }

            var checksum = string.Concat(letterCounts
                .OrderByDescending(pair => pair.Value)
                .ThenBy(pair => pair.Key)
                .Take(5)
                .Select(pair => pair.Key));

            if (checksum == givenChecksum) sectorIdSum += sectorId;
        }

        return sectorIdSum;
    }

    private static int SolvePartTwo() =>
        (from room in Data
            select SplitDetails().Match(room)
            into match
            where match.Success
            let encryptedName = match.Groups[1].Value
            let sectorId = int.Parse(match.Groups[2].Value)
            let decryptedName = DecryptName(encryptedName, sectorId)
            where decryptedName.Contains("northpole object")
            select sectorId).FirstOrDefault();

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    [GeneratedRegex(@"([a-z-]+)(\d+)\[([a-z]+)\]")]
    private static partial Regex SplitDetails();
}