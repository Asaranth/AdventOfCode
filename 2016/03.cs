namespace _2016;

/// <summary>
/// Day 03: Squares With Three Sides
/// 
/// Validates triangle side specifications given row-wise and column-wise.
/// </summary>
public abstract class _03
{
    private static readonly string[] Data;

    static _03() => Data = Task.Run(() => Utils.GetInputData(3)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Checks whether three side lengths can form a valid triangle using the triangle inequality theorem.
    /// </summary>
    /// <param name="sides">Array of three side lengths.</param>
    /// <returns>True if the sides form a valid triangle; otherwise, false.</returns>
    private static bool ValidTriangle(int[] sides) =>
        sides[0] + sides[1] > sides[2] && sides[1] + sides[2] > sides[0] && sides[2] + sides[0] > sides[1];

    /// <summary>
    /// Solves Part One: counts valid triangles when specifications are listed row by row.
    /// </summary>
    /// <returns>Count of valid triangles in Part One.</returns>
    private static int SolvePartOne() =>
        Data.Select(line =>
                line.Trim().Split([' '], StringSplitOptions.RemoveEmptyEntries).Select(int.Parse).ToArray())
            .Count(ValidTriangle);

    /// <summary>
    /// Solves Part Two: counts valid triangles when specifications are grouped in vertical 3x3 column batches.
    /// </summary>
    /// <returns>Count of valid triangles in Part Two.</returns>
    private static int SolvePartTwo()
    {
        var validTriangleCount = 0;
        var columns = new List<List<int>> { new(), new(), new() };

        for (var row = 0; row < Data.Length; row += 3)
        {
            var firstRow = Data[row].Trim().Split([' '], StringSplitOptions.RemoveEmptyEntries)
                .Select(int.Parse).ToArray();
            var secondRow = Data[row + 1].Trim().Split([' '], StringSplitOptions.RemoveEmptyEntries)
                .Select(int.Parse).ToArray();
            var thirdRow = Data[row + 2].Trim().Split([' '], StringSplitOptions.RemoveEmptyEntries)
                .Select(int.Parse).ToArray();

            for (var col = 0; col < 3; col++)
            {
                columns[col].Add(firstRow[col]);
                columns[col].Add(secondRow[col]);
                columns[col].Add(thirdRow[col]);
            }
        }

        foreach (var col in columns)
        {
            for (var i = 0; i < col.Count; i += 3)
            {
                var sides = new[] { col[i], col[i + 1], col[i + 2] };
                if (ValidTriangle(sides)) validTriangleCount++;
            }
        }

        return validTriangleCount;
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