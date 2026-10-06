namespace _2016;

/// <summary>
/// Day 09: Explosives in Cyberspace
/// 
/// Calculates decompressed data lengths using run-length marker expansion with non-recursive and recursive evaluation.
/// </summary>
public abstract class _09
{
    private static readonly string Data;

    static _09() => Data = string.Concat(Task.Run(() => Utils.GetInputData(9)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries)).Replace(" ", "");

    /// <summary>
    /// Computes decompressed length of a substring using compression markers (AxB).
    /// </summary>
    /// <param name="input">Compressed data string.</param>
    /// <param name="start">Start character index.</param>
    /// <param name="end">End character index.</param>
    /// <param name="recursive">If true, recursively expands nested markers (Version Two format).</param>
    /// <returns>The total decompressed length.</returns>
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

    /// <summary>
    /// Solves Part One: calculates decompressed length ignoring markers contained in data expansions.
    /// </summary>
    /// <returns>Decompressed character length for Part One.</returns>
    private static long SolvePartOne() => CalculateDecompressedLength(Data, 0, Data.Length, false);

    /// <summary>
    /// Solves Part Two: calculates decompressed length with recursive marker expansion.
    /// </summary>
    /// <returns>Decompressed character length for Part Two.</returns>
    private static long SolvePartTwo() => CalculateDecompressedLength(Data, 0, Data.Length, true);

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}