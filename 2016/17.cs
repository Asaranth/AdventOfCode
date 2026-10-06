using System.Security.Cryptography;
using System.Text;

namespace _2016;

/// <summary>
/// Day 17: Two Steps Forward
/// 
/// Navigates a 4x4 vault grid with dynamic MD5-hash-controlled doors to find shortest and longest valid paths.
/// </summary>
public static class _17
{
    private static readonly string Data;

    static _17() => Data = Task.Run(() => Utils.GetInputData(17)).Result.Trim();

    /// <summary>
    /// Finds the shortest alphanumeric path from (0, 0) to vault room (3, 3).
    /// </summary>
    /// <returns>Direction string of shortest path.</returns>
    private static string FindShortestPath()
    {
        var directions = new[] { 'U', 'D', 'L', 'R' };
        var queue = new Queue<State>();
        queue.Enqueue(new State(0, 0, Data));

        while (queue.Count > 0)
        {
            var currentState = queue.Dequeue();
            if (currentState is { X: 3, Y: 3 }) return currentState.Path[Data.Length..];
            var hash = GetMd5Hash(currentState.Path);
            for (var i = 0; i < 4; i++)
            {
                if (!IsOpen(hash[i])) continue;
                var (newX, newY) = Move(currentState.X, currentState.Y, directions[i]);
                if (IsValid(newX, newY)) queue.Enqueue(new State(newX, newY, currentState.Path + directions[i]));
            }
        }

        return string.Empty;
    }

    /// <summary>
    /// Finds the length of the longest path reaching (3, 3) without looping past the vault door.
    /// </summary>
    /// <returns>Step length of the longest path.</returns>
    private static int FindLongestPathLength()
    {
        var directions = new[] { 'U', 'D', 'L', 'R' };
        var queue = new Queue<State>();
        queue.Enqueue(new State(0, 0, Data));
        var longestPathLength = 0;

        while (queue.Count > 0)
        {
            var currentState = queue.Dequeue();
            if (currentState is { X: 3, Y: 3 })
            {
                var pathLength = currentState.Path.Length - Data.Length;
                if (pathLength > longestPathLength) longestPathLength = pathLength;
                continue;
            }

            var hash = GetMd5Hash(currentState.Path);
            for (var i = 0; i < 4; i++)
            {
                if (!IsOpen(hash[i])) continue;
                var (newX, newY) = Move(currentState.X, currentState.Y, directions[i]);
                if (IsValid(newX, newY)) queue.Enqueue(new State(newX, newY, currentState.Path + directions[i]));
            }
        }

        return longestPathLength;
    }

    /// <summary>
    /// Translates coordinates according to the chosen movement direction.
    /// </summary>
    /// <param name="x">Current X coordinate.</param>
    /// <param name="y">Current Y coordinate.</param>
    /// <param name="direction">Direction character ('U', 'D', 'L', 'R').</param>
    /// <returns>New (X, Y) coordinate pair.</returns>
    private static (int, int) Move(int x, int y, char direction) => direction switch
    {
        'U' => (x, y - 1),
        'D' => (x, y + 1),
        'L' => (x - 1, y),
        'R' => (x + 1, y),
        _ => (x, y)
    };

    /// <summary>
    /// Checks whether the coordinates lie within the 4x4 grid bounds.
    /// </summary>
    /// <param name="x">X coordinate.</param>
    /// <param name="y">Y coordinate.</param>
    /// <returns>True if within [0, 3] bounds; otherwise, false.</returns>
    private static bool IsValid(int x, int y) => x is >= 0 and < 4 && y is >= 0 and < 4;

    /// <summary>
    /// Determines whether a door is open based on the MD5 hash character (open if 'b', 'c', 'd', 'e', or 'f').
    /// </summary>
    /// <param name="c">Hash character corresponding to the door.</param>
    /// <returns>True if the door is unlocked and open; otherwise, false.</returns>
    private static bool IsOpen(char c) => "bcdef".Contains(c);

    /// <summary>
    /// Computes the MD5 hex string for the current passcode and path.
    /// </summary>
    /// <param name="input">Path string.</param>
    /// <returns>Lowercase hex digest.</returns>
    private static string GetMd5Hash(string input)
    {
        var hashBytes = MD5.HashData(Encoding.ASCII.GetBytes(input));
        var sb = new StringBuilder();
        foreach (var b in hashBytes) sb.Append(b.ToString("x2"));
        return sb.ToString();
    }

    /// <summary>
    /// State class tracking position and path history.
    /// </summary>
    private class State(int x, int y, string path)
    {
        public int X { get; } = x;
        public int Y { get; } = y;
        public string Path { get; } = path;
    }

    /// <summary>
    /// Solves Part One: finds the shortest path reaching the vault room.
    /// </summary>
    /// <returns>Shortest path string.</returns>
    private static string SolvePartOne() => FindShortestPath();

    /// <summary>
    /// Solves Part Two: calculates the length of the longest path reaching the vault.
    /// </summary>
    /// <returns>Maximum step count.</returns>
    private static int SolvePartTwo() => FindLongestPathLength();

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}