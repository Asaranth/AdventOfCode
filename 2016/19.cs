namespace _2016;

public static class _19
{
    private static readonly int Data;

    static _19() => Data = int.Parse(Task.Run(() => Utils.GetInputData(19)).Result.Trim());

    private static int SolvePartOne()
    {
        var largestPowerOf2 = 1;
        while (largestPowerOf2 <= Data) largestPowerOf2 <<= 1;
        largestPowerOf2 >>= 1;
        return 2 * (Data - largestPowerOf2) + 1;
    }

    private static int SolvePartTwo()
    {
        var left = new LinkedList<int>();
        var right = new LinkedList<int>();

        for (var i = 1; i <= Data; i++)
        {
            if (i <= Data / 2) left.AddLast(i);
            else right.AddLast(i);
        }

        while (left.Count + right.Count > 1)
        {
            if (left.Count > right.Count) left.RemoveLast();
            else right.RemoveFirst();

            right.AddLast(GetFirstValue(left));
            left.RemoveFirst();
            left.AddLast(GetFirstValue(right));
            right.RemoveFirst();
        }

        return GetFirstValue(left.Count > 0 ? left : right);

        int GetFirstValue(LinkedList<int> list) =>
            list.First?.Value ?? throw new InvalidOperationException("The linked list should not be empty");
    }

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}