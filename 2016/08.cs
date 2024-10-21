using System.Text.RegularExpressions;

namespace _2016;

public abstract partial class _08
{
    private static readonly string[] Data;

    static _08() => Data = Task.Run(() => Utils.GetInputData(8)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    private const int Width = 50;
    private const int Height = 6;

    private static void CreateRect(bool[,] screen, int width, int height)
    {
        for (var y = 0; y < height; y++)
        for (var x = 0; x < width; x++)
            screen[y, x] = true;
    }

    private static void RotateRow(bool[,] screen, int row, int amount)
    {
        var newRow = new bool[Width];
        for (var x = 0; x < Width; x++) newRow[(x + amount) % Width] = screen[row, x];
        for (var x = 0; x < Width; x++) screen[row, x] = newRow[x];
    }

    private static void RotateCol(bool[,] screen, int col, int amount)
    {
        var newCol = new bool[Height];
        for (var y = 0; y < Height; y++) newCol[(y + amount) % Height] = screen[y, col];
        for (var y = 0; y < Height; y++) screen[y, col] = newCol[y];
    }

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

    private static int SolvePartOne()
    {
        var screen = new bool[Height, Width];
        foreach (var instruction in Data) ExecuteInstruction(screen, instruction);
        return screen.Cast<bool>().Count(pixel => pixel);
    }

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

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    [GeneratedRegex(@"rect (\d+)x(\d+)")]
    private static partial Regex CreateRectInstruction();

    [GeneratedRegex(@"rotate row y=(\d+) by (\d+)")]
    private static partial Regex RotateRowInstruction();

    [GeneratedRegex(@"rotate column x=(\d+) by (\d+)")]
    private static partial Regex RotateColInstruction();
}