using System.Text.RegularExpressions;

namespace _2016;

/// <summary>
/// Day 21: Scrambled Letters and Hash
/// 
/// Applies reversible string scrambling operations including positional swaps, circular rotations, letter-based rotations, reversals, and moves.
/// </summary>
public static partial class _21
{
    private static readonly string[] Data;

    static _21() => Data = Task.Run(() => Utils.GetInputData(21)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Swaps the characters located at indices x and y.
    /// </summary>
    /// <param name="input">Source string.</param>
    /// <param name="x">First index.</param>
    /// <param name="y">Second index.</param>
    /// <returns>Resulting string after index swap.</returns>
    private static string SwapPosition(string input, int x, int y)
    {
        var chars = input.ToCharArray();
        (chars[x], chars[y]) = (chars[y], chars[x]);
        return new string(chars);
    }

    /// <summary>
    /// Swaps all occurrences of letter x with letter y and vice versa.
    /// </summary>
    /// <param name="input">Source string.</param>
    /// <param name="x">First character.</param>
    /// <param name="y">Second character.</param>
    /// <returns>Resulting string after character substitution.</returns>
    private static string SwapLetter(string input, char x, char y) =>
        new(input.Select(c => c == x ? y : c == y ? x : c).ToArray());

    /// <summary>
    /// Rotates string characters circularly to the left or right by a specified step count.
    /// </summary>
    /// <param name="input">Source string.</param>
    /// <param name="steps">Number of positions to rotate.</param>
    /// <param name="left">If true, rotates left; if false, rotates right.</param>
    /// <returns>Rotated string.</returns>
    private static string Rotate(string input, int steps, bool left)
    {
        var len = input.Length;
        steps = (steps % len + len) % len;
        return left
            ? string.Concat(input.AsSpan()[steps..], input.AsSpan(0, steps))
            : string.Concat(input.AsSpan()[(len - steps)..], input.AsSpan(0, len - steps));
    }

    /// <summary>
    /// Rotates the entire string right based on the index of character x.
    /// </summary>
    /// <param name="input">Source string.</param>
    /// <param name="x">Target character.</param>
    /// <returns>Rotated string.</returns>
    private static string RotateBasedOnPosition(string input, char x)
    {
        var index = input.IndexOf(x);
        var steps = 1 + index + (index >= 4 ? 1 : 0);
        return Rotate(input, steps, false);
    }

    /// <summary>
    /// Inverts the right-rotation based on character position using a precalculated inverse rotation lookup for length 8.
    /// </summary>
    /// <param name="input">Source string.</param>
    /// <param name="x">Target character.</param>
    /// <returns>Unrotated string.</returns>
    private static string RotateBasedOnPositionReversed(string input, char x)
    {
        var index = input.IndexOf(x);
        int[] lookup = [1, 1, 6, 2, 7, 3, 0, 4];
        return Rotate(input, lookup[index], true);
    }

    /// <summary>
    /// Reverses the substring between index x and index y (inclusive).
    /// </summary>
    /// <param name="input">Source string.</param>
    /// <param name="x">Start index.</param>
    /// <param name="y">End index.</param>
    /// <returns>String with reversed segment.</returns>
    private static string ReversePositions(string input, int x, int y)
    {
        var segment = input.Substring(x, y - x + 1).Reverse().ToArray();
        return string.Concat(input.AsSpan(0, x), new string(segment), input.AsSpan(y + 1));
    }

    /// <summary>
    /// Removes character at index x and inserts it at index y.
    /// </summary>
    /// <param name="input">Source string.</param>
    /// <param name="x">Origin index.</param>
    /// <param name="y">Destination index.</param>
    /// <returns>Modified string.</returns>
    private static string MovePosition(string input, int x, int y) => input.Remove(x, 1).Insert(y, input[x].ToString());

    /// <summary>
    /// Inverts the move operation by moving from index y to index x.
    /// </summary>
    /// <param name="input">Source string.</param>
    /// <param name="x">Original origin index.</param>
    /// <param name="y">Original destination index.</param>
    /// <returns>Reverted string.</returns>
    private static string MovePositionReversed(string input, int x, int y) => MovePosition(input, y, x);

    /// <summary>
    /// Parses and applies a single scrambling or unscrambling rule.
    /// </summary>
    /// <param name="instruction">Instruction line string.</param>
    /// <param name="input">Current string state.</param>
    /// <param name="reverse">If true, executes the reverse operation.</param>
    /// <returns>Transformed string.</returns>
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

    /// <summary>
    /// Scrambles an input string through all rules in sequential order.
    /// </summary>
    /// <param name="input">Initial un-scrambled string.</param>
    /// <returns>Scrambled result.</returns>
    private static string Scramble(string input) =>
        Data.Aggregate(input, (current, line) => ApplyInstruction(line, current));

    /// <summary>
    /// Unscrambles an input string by applying inverse operations in reverse order.
    /// </summary>
    /// <param name="input">Scrambled hash string.</param>
    /// <returns>Decrypted original string.</returns>
    private static string Unscramble(string input) => Data.Reverse()
        .Aggregate(input, (current, line) => ApplyInstruction(line, current, reverse: true));

    /// <summary>
    /// Solves Part One: scrambles "abcdefgh".
    /// </summary>
    /// <returns>The scrambled string.</returns>
    private static string SolvePartOne() => Scramble("abcdefgh");

    /// <summary>
    /// Solves Part Two: unscrambles "fbgdceah".
    /// </summary>
    /// <returns>The unscrambled password string.</returns>
    private static string SolvePartTwo() => Unscramble("fbgdceah");

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    /// <summary>
    /// Regex pattern matching "swap position X with position Y".
    /// </summary>
    [GeneratedRegex(@"swap position (\d+) with position (\d+)")]
    private static partial Regex SwapPositionRegex();

    /// <summary>
    /// Regex pattern matching "swap letter X with letter Y".
    /// </summary>
    [GeneratedRegex(@"swap letter (\w) with letter (\w)")]
    private static partial Regex SwapLetterRegex();

    /// <summary>
    /// Regex pattern matching "rotate (left|right) X steps".
    /// </summary>
    [GeneratedRegex(@"rotate (left|right) (\d+) steps?")]
    private static partial Regex RotateStepsRegex();

    /// <summary>
    /// Regex pattern matching "rotate based on position of letter X".
    /// </summary>
    [GeneratedRegex(@"rotate based on position of letter (\w)")]
    private static partial Regex RotatePositionRegex();

    /// <summary>
    /// Regex pattern matching "reverse positions X through Y".
    /// </summary>
    [GeneratedRegex(@"reverse positions (\d+) through (\d+)")]
    private static partial Regex ReversePositionRegex();

    /// <summary>
    /// Regex pattern matching "move position X to position Y".
    /// </summary>
    [GeneratedRegex(@"move position (\d+) to position (\d+)")]
    private static partial Regex MovePositionRegex();
}