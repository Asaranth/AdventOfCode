namespace _2016;

public abstract class _09
{
    private static readonly string Data;

    static _09() => Data = string.Concat(Task.Run(() => Utils.GetInputData(9)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries)).Replace(" ", "");

    private static long CalculateDecompressedLength(string input, int start, int end, bool recursive)
    {
        long decomLen = 0;

        for (var i = start; i < end;)
        {
            if (input[i] == '(')
            {
                var markerEnd = input.IndexOf(')', i);
                if (markerEnd == -1) break;

                var marker = input.Substring(i + 1, markerEnd - i - 1);
                var parts = marker.Split('x');
                if (parts.Length != 2 || !int.TryParse(parts[0], out var seqLen) ||
                    !int.TryParse(parts[1], out var repCount)) break;

                var subStart = markerEnd + 1;
                var subEnd = subStart + seqLen;

                if (recursive) decomLen += repCount * CalculateDecompressedLength(input, subStart, subEnd, true);
                else decomLen += seqLen * repCount;

                i = subEnd;
            }
            else
            {
                decomLen++;
                i++;
            }
        }

        return decomLen;
    }

    private static long SolvePartOne() => CalculateDecompressedLength(Data, 0, Data.Length, false);

    private static long SolvePartTwo() => CalculateDecompressedLength(Data, 0, Data.Length, true);

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}