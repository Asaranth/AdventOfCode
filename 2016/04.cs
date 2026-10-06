using System.Text.RegularExpressions;

namespace _2016;

/// <summary>
/// Day 04: Security Through Obscurity
/// 
/// Validates room checksums using character frequencies and decrypts room names via Caesar cipher shift.
/// </summary>
public abstract partial class _04
{
    private static readonly string[] Data;

    static _04() => Data = Task.Run(() => Utils.GetInputData(4)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Decrypts a room name by rotating each letter forward through the alphabet by its sector ID.
    /// </summary>
    /// <param name="encryptedName">The hyphen-separated encrypted name string.</param>
    /// <param name="sectorId">The sector ID shift value.</param>
    /// <returns>The decrypted room name.</returns>
    private static string DecryptName(string encryptedName, int sectorId) =>
        string.Concat(encryptedName.Select(ch => ch == '-' ? ' ' : (char)('a' + (ch - 'a' + sectorId) % 26)));

    /// <summary>
    /// Solves Part One: verifies room checksums against top five most common letters and sums valid sector IDs.
    /// </summary>
    /// <returns>Sum of sector IDs of all real rooms.</returns>
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

    /// <summary>
    /// Solves Part Two: decrypts room names and identifies the sector ID of the room where North Pole objects are stored.
    /// </summary>
    /// <returns>Sector ID of the North Pole storage room.</returns>
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

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    /// <summary>
    /// Regular expression pattern matching room name components: name, sector ID, and checksum.
    /// </summary>
    [GeneratedRegex(@"([a-z-]+)(\d+)\[([a-z]+)\]")]
    private static partial Regex SplitDetails();
}