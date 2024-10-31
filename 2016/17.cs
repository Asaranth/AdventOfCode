using System.Security.Cryptography;
using System.Text;

namespace _2016;

public static class _17
{
    private static readonly string Data;

    static _17() => Data = Task.Run(() => Utils.GetInputData(17)).Result.Trim();

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

    private static (int, int) Move(int x, int y, char direction) => direction switch
    {
        'U' => (x, y - 1),
        'D' => (x, y + 1),
        'L' => (x - 1, y),
        'R' => (x + 1, y),
        _ => (x, y)
    };

    private static bool IsValid(int x, int y) => x is >= 0 and < 4 && y is >= 0 and < 4;

    private static bool IsOpen(char c) => "bcdef".Contains(c);

    private static string GetMd5Hash(string input)
    {
        var hashBytes = MD5.HashData(Encoding.ASCII.GetBytes(input));
        var sb = new StringBuilder();
        foreach (var b in hashBytes) sb.Append(b.ToString("x2"));
        return sb.ToString();
    }

    private class State(int x, int y, string path)
    {
        public int X { get; } = x;
        public int Y { get; } = y;
        public string Path { get; } = path;
    }

    private static string SolvePartOne() => FindShortestPath();

    private static int SolvePartTwo() => FindLongestPathLength();

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}