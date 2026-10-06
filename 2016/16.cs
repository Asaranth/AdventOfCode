using System.Text;

namespace _2016;

/// <summary>
/// Day 16: Dragon Checksum
/// 
/// Generates pseudo-random binary disk data using a modified dragon curve and calculates recursive parity checksums.
/// </summary>
public static class _16
{
    private static readonly string Data;

    static _16() => Data = Task.Run(() => Utils.GetInputData(16)).Result.Trim();

    /// <summary>
    /// Generates the next dragon curve expansion for string 'a' by appending '0' and inverted reversed 'a'.
    /// </summary>
    /// <param name="a">Source binary string.</param>
    /// <returns>The expanded binary string.</returns>
    private static string GenerateDragonCurve(string a)
    {
        var b = new string(a.Reverse().ToArray());
        b = new string(b.Select(ch => ch == '0' ? '1' : '0').ToArray());
        return a + '0' + b;
    }

    /// <summary>
    /// Computes the parity checksum for a binary data string by repeatedly compressing pairs until an odd-length checksum is obtained.
    /// </summary>
    /// <param name="data">Binary string to checksum.</param>
    /// <returns>The odd-length binary checksum string.</returns>
    private static string CalculateChecksum(string data)
    {
        while (data.Length % 2 == 0)
        {
            var checksum = new StringBuilder();
            for (var i = 0; i < data.Length; i += 2) checksum.Append(data[i] == data[i + 1] ? '1' : '0');
            data = checksum.ToString();
        }

        return data;
    }

    /// <summary>
    /// Expands the seed data to fill the specified disk capacity and generates its final checksum.
    /// </summary>
    /// <param name="diskLength">Target disk capacity in bits.</param>
    /// <returns>Final odd-length checksum.</returns>
    private static string GetDiskChecksum(int diskLength)
    {
        var data = Data;
        while (data.Length < diskLength) data = GenerateDragonCurve(data);
        data = data[..diskLength];
        return CalculateChecksum(data);
    }

    /// <summary>
    /// Solves Part One: fills a disk of length 272 and computes its checksum.
    /// </summary>
    /// <returns>Checksum string for Part One.</returns>
    private static string SolvePartOne() => GetDiskChecksum(272);

    /// <summary>
    /// Solves Part Two: fills a disk of length 35,651,584 and computes its checksum.
    /// </summary>
    /// <returns>Checksum string for Part Two.</returns>
    private static string SolvePartTwo() => GetDiskChecksum(35651584);

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}