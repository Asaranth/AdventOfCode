namespace _2016;

/// <summary>
/// Day 19: An Elephant Named Joseph
/// 
/// Solves the Josephus gift-stealing circle problem using power-of-2 arithmetic and split linked lists.
/// </summary>
public static class _19
{
    private static readonly int Data;

    static _19() => Data = int.Parse(Task.Run(() => Utils.GetInputData(19)).Result.Trim());

    /// <summary>
    /// Solves Part One: finds the winning elf in the standard next-neighbour Josephus circle (L = 2(n - 2^k) + 1).
    /// </summary>
    /// <returns>1-based index of the elf who gets all presents.</returns>
    private static int SolvePartOne()
    {
        var largestPowerOf2 = 1;
        while (largestPowerOf2 <= Data) largestPowerOf2 <<= 1;
        largestPowerOf2 >>= 1;
        return 2 * (Data - largestPowerOf2) + 1;
    }

    /// <summary>
    /// Solves Part Two: finds the winning elf when stealing from the elf directly across the circle.
    /// </summary>
    /// <returns>1-based index of the winning elf in Part Two.</returns>
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

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}