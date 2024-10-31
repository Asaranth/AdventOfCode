using System.Text.RegularExpressions;

namespace _2016;

public static partial class _21
{
    private static readonly string[] Data;

    static _21() => Data = Task.Run(() => Utils.GetInputData(21)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    private static string SwapPosition(string input, int x, int y)
    {
        var chars = input.ToCharArray();
        (chars[x], chars[y]) = (chars[y], chars[x]);
        return new string(chars);
    }

    private static string SwapLetter(string input, char x, char y) =>
        new(input.Select(c => c == x ? y : c == y ? x : c).ToArray());

    private static string Rotate(string input, int steps, bool left)
    {
        var len = input.Length;
        steps = (steps % len + len) % len;
        return left
            ? string.Concat(input.AsSpan()[steps..], input.AsSpan(0, steps))
            : string.Concat(input.AsSpan()[(len - steps)..], input.AsSpan(0, len - steps));
    }

    private static string RotateBasedOnPosition(string input, char x)
    {
        var index = input.IndexOf(x);
        var steps = 1 + index + (index >= 4 ? 1 : 0);
        return Rotate(input, steps, false);
    }

    private static string RotateBasedOnPositionReversed(string input, char x)
    {
        var index = input.IndexOf(x);
        int[] lookup = [1, 1, 6, 2, 7, 3, 0, 4];
        return Rotate(input, lookup[index], true);
    }

    private static string ReversePositions(string input, int x, int y)
    {
        var segment = input.Substring(x, y - x + 1).Reverse().ToArray();
        return string.Concat(input.AsSpan(0, x), new string(segment), input.AsSpan(y + 1));
    }

    private static string MovePosition(string input, int x, int y) => input.Remove(x, 1).Insert(y, input[x].ToString());

    private static string MovePositionReversed(string input, int x, int y) => MovePosition(input, y, x);

    private static string ApplyInstruction(string instruction, string input, bool reverse = false)
    {
        var swapPositionMatch = SwapPositionRegex().Match(instruction);
        var swapLetterMatch = SwapLetterRegex().Match(instruction);
        var rotateMatch = RotateStepsRegex().Match(instruction);
        var rotateBasedOnMatch = RotatePositionRegex().Match(instruction);
        var reverseMatch = ReversePositionRegex().Match(instruction);
        var moveMatch = MovePositionRegex().Match(instruction);

        if (swapPositionMatch.Success)
            return SwapPosition(input, int.Parse(swapPositionMatch.Groups[1].Value),
                int.Parse(swapPositionMatch.Groups[2].Value));

        if (swapLetterMatch.Success)
            return SwapLetter(input, swapLetterMatch.Groups[1].Value[0], swapLetterMatch.Groups[2].Value[0]);

        if (rotateMatch.Success)
        {
            var left = rotateMatch.Groups[1].Value == "left";
            if (reverse) left = !left;
            return Rotate(input, int.Parse(rotateMatch.Groups[2].Value), left);
        }

        if (rotateBasedOnMatch.Success)
            return reverse
                ? RotateBasedOnPositionReversed(input, rotateBasedOnMatch.Groups[1].Value[0])
                : RotateBasedOnPosition(input, rotateBasedOnMatch.Groups[1].Value[0]);

        if (reverseMatch.Success)
            return ReversePositions(input, int.Parse(reverseMatch.Groups[1].Value),
                int.Parse(reverseMatch.Groups[2].Value));

        if (!moveMatch.Success) return input;

        if (reverse)
            return MovePositionReversed(input, int.Parse(moveMatch.Groups[1].Value),
                int.Parse(moveMatch.Groups[2].Value));

        return MovePosition(input, int.Parse(moveMatch.Groups[1].Value),
            int.Parse(moveMatch.Groups[2].Value));
    }

    private static string Scramble(string input) =>
        Data.Aggregate(input, (current, line) => ApplyInstruction(line, current));

    private static string Unscramble(string input) => Data.Reverse()
        .Aggregate(input, (current, line) => ApplyInstruction(line, current, reverse: true));

    private static string SolvePartOne() => Scramble("abcdefgh");

    private static string SolvePartTwo() => Unscramble("fbgdceah");

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    [GeneratedRegex(@"swap position (\d+) with position (\d+)")]
    private static partial Regex SwapPositionRegex();

    [GeneratedRegex(@"swap letter (\w) with letter (\w)")]
    private static partial Regex SwapLetterRegex();

    [GeneratedRegex(@"rotate (left|right) (\d+) steps?")]
    private static partial Regex RotateStepsRegex();

    [GeneratedRegex(@"rotate based on position of letter (\w)")]
    private static partial Regex RotatePositionRegex();

    [GeneratedRegex(@"reverse positions (\d+) through (\d+)")]
    private static partial Regex ReversePositionRegex();

    [GeneratedRegex(@"move position (\d+) to position (\d+)")]
    private static partial Regex MovePositionRegex();
}