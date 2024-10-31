using System.Text;

namespace _2016;

public static class _16
{
    private static readonly string Data;

    static _16() => Data = Task.Run(() => Utils.GetInputData(16)).Result.Trim();

    private static string GenerateDragonCurve(string a)
    {
        var b = new string(a.Reverse().ToArray());
        b = new string(b.Select(ch => ch == '0' ? '1' : '0').ToArray());
        return a + '0' + b;
    }

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

    private static string GetDiskChecksum(int diskLength)
    {
        var data = Data;
        while (data.Length < diskLength) data = GenerateDragonCurve(data);
        data = data[..diskLength];
        return CalculateChecksum(data);
    }

    private static string SolvePartOne() => GetDiskChecksum(272);

    private static string SolvePartTwo() => GetDiskChecksum(35651584);

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}