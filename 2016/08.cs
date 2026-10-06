using System.Text.RegularExpressions;

namespace _2016;

/// <summary>
/// Day 08: Two-Factor Authentication
/// 
/// Simulates a 50x6 pixel screen supporting rectangular block filling and circular row/column rotations.
/// </summary>
public abstract partial class _08
{
    private static readonly string[] Data;

    static _08() => Data = Task.Run(() => Utils.GetInputData(8)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    private const int Width = 50;
    private const int Height = 6;

    /// <summary>
    /// Activates an AxB rectangle of pixels at the top-left corner of the screen.
    /// </summary>
    /// <param name="screen">2D boolean pixel matrix.</param>
    /// <param name="width">Rectangle width (A).</param>
    /// <param name="height">Rectangle height (B).</param>
    private static void CreateRect(bool[,] screen, int width, int height)
    {
        for (var y = 0; y < height; y++)
        for (var x = 0; x < width; x++)
            screen[y, x] = true;
    }

    /// <summary>
    /// Rotates all pixels in a specific screen row circularly to the right by the specified amount.
    /// </summary>
    /// <param name="screen">2D boolean pixel matrix.</param>
    /// <param name="row">Row index.</param>
    /// <param name="amount">Shift amount.</param>
    private static void RotateRow(bool[,] screen, int row, int amount)
    {
        var newRow = new bool[Width];
        for (var x = 0; x < Width; x++) newRow[(x + amount) % Width] = screen[row, x];
        for (var x = 0; x < Width; x++) screen[row, x] = newRow[x];
    }

    /// <summary>
    /// Rotates all pixels in a specific screen column circularly downwards by the specified amount.
    /// </summary>
    /// <param name="screen">2D boolean pixel matrix.</param>
    /// <param name="col">Column index.</param>
    /// <param name="amount">Shift amount.</param>
    private static void RotateCol(bool[,] screen, int col, int amount)
    {
        var newCol = new bool[Height];
        for (var y = 0; y < Height; y++) newCol[(y + amount) % Height] = screen[y, col];
        for (var y = 0; y < Height; y++) screen[y, col] = newCol[y];
    }

    /// <summary>
    /// Parses and applies a screen manipulation instruction (rect, rotate row, or rotate column).
    /// </summary>
    /// <param name="screen">2D boolean pixel matrix.</param>
    /// <param name="instruction">Instruction line string.</param>
    private static void ExecuteInstruction(bool[,] screen, string instruction)
    {
        if (instruction.StartsWith("rect"))
        {
            var match = CreateRectInstruction().Match(instruction);
            var width = int.Parse(match.Groups[1].Value);
            var height = int.Parse(match.Groups[2].Value);
            CreateRect(screen, width, height);
        }
        else if (instruction.StartsWith("rotate row"))
        {
            var match = RotateRowInstruction().Match(instruction);
            var row = int.Parse(match.Groups[1].Value);
            var amount = int.Parse(match.Groups[2].Value);
            RotateRow(screen, row, amount);
        }
        else if (instruction.StartsWith("rotate column"))
        {
            var match = RotateColInstruction().Match(instruction);
            var col = int.Parse(match.Groups[1].Value);
            var amount = int.Parse(match.Groups[2].Value);
            RotateCol(screen, col, amount);
        }
    }

    /// <summary>
    /// Solves Part One: executes instructions and counts the total number of illuminated pixels.
    /// </summary>
    /// <returns>Count of active pixels.</returns>
    private static int SolvePartOne()
    {
        var screen = new bool[Height, Width];
        foreach (var instruction in Data) ExecuteInstruction(screen, instruction);
        return screen.Cast<bool>().Count(pixel => pixel);
    }

    /// <summary>
    /// Solves Part Two: renders the screen grid to display the resulting alphanumeric code.
    /// </summary>
    /// <returns>Instruction message indicating visual letter readout.</returns>
    private static string SolvePartTwo()
    {
        var screen = new bool[Height, Width];
        foreach (var instruction in Data) ExecuteInstruction(screen, instruction);
        for (var y = 0; y < Height; y++)
        {
            for (var x = 0; x < Width; x++) Console.Write(screen[y, x] ? '█' : ' ');
            Console.WriteLine();
        }
        return "Manually decode letters";
    }

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    /// <summary>
    /// Regex pattern for parsing "rect AxB" commands.
    /// </summary>
    [GeneratedRegex(@"rect (\d+)x(\d+)")]
    private static partial Regex CreateRectInstruction();

    /// <summary>
    /// Regex pattern for parsing "rotate row y=A by B" commands.
    /// </summary>
    [GeneratedRegex(@"rotate row y=(\d+) by (\d+)")]
    private static partial Regex RotateRowInstruction();

    /// <summary>
    /// Regex pattern for parsing "rotate column x=A by B" commands.
    /// </summary>
    [GeneratedRegex(@"rotate column x=(\d+) by (\d+)")]
    private static partial Regex RotateColInstruction();
}