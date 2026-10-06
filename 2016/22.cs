using System.Text.RegularExpressions;

namespace _2016;

/// <summary>
/// Day 22: Grid Computing
/// 
/// Analyzes storage grid cluster filesystem nodes to find viable transfer pairs and calculates sliding tile moves for data extraction.
/// </summary>
public static partial class _22
{
    private static readonly string[] Data;

    static _22() => Data = Task.Run(() => Utils.GetInputData(22)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Parses df-like filesystem output lines into a list of storage Node instances.
    /// </summary>
    /// <returns>List of parsed nodes.</returns>
    private static List<Node> ParseNodes()
    {
        var regex = MyRegex();
        return (from line in Data
            select regex.Match(line)
            into match
            where match.Success
            let x = int.Parse(match.Groups[1].Value)
            let y = int.Parse(match.Groups[2].Value)
            let size = int.Parse(match.Groups[3].Value)
            let used = int.Parse(match.Groups[4].Value)
            let avail = int.Parse(match.Groups[5].Value)
            select new Node(x, y, size, used, avail)).ToList();
    }

    /// <summary>
    /// Generates orthogonal neighbouring coordinates within grid boundaries.
    /// </summary>
    /// <param name="x">Current X coordinate.</param>
    /// <param name="y">Current Y coordinate.</param>
    /// <param name="width">Grid width.</param>
    /// <param name="height">Grid height.</param>
    /// <returns>Enumeration of adjacent coordinate tuples.</returns>
    private static IEnumerable<(int x, int y)> GetNeighbors(int x, int y, int width, int height)
    {
        if (x > 0) yield return (x - 1, y);
        if (x < width - 1) yield return (x + 1, y);
        if (y > 0) yield return (x, y - 1);
        if (y < height - 1) yield return (x, y + 1);
    }

    /// <summary>
    /// Represents a grid computing filesystem node with position coordinates and capacity statistics.
    /// </summary>
    private class Node(int x, int y, int size, int used, int avail)
    {
        public int X { get; } = x;
        public int Y { get; } = y;
        public int Size { get; } = size;
        public int Used { get; } = used;
        public int Avail { get; } = avail;
    }

    /// <summary>
    /// Solves Part One: counts viable pairs of nodes (A != B, A not empty, A.Used &lt;= B.Avail).
    /// </summary>
    /// <returns>Total number of viable node pairs.</returns>
    private static int SolvePartOne()
    {
        var nodes = ParseNodes();
        return nodes.Select((s, i) => nodes.Where((t, j) => i != j && s.Used > 0 && s.Used <= t.Avail).Count()).Sum();
    }

    /// <summary>
    /// Solves Part Two: calculates fewest steps to move the goal data from top-right to (0, 0) around impassable high-capacity nodes.
    /// </summary>
    /// <returns>Minimum step count.</returns>
    private static int SolvePartTwo()
    {
        var nodes = ParseNodes();
        var emptyNode = nodes.First(node => node.Used == 0);
        var goalNode = nodes.First(node => node.Y == 0 && node.X == nodes.Max(n => n.X));
        var gridWidth = nodes.Max(n => n.X) + 1;
        var gridHeight = nodes.Max(n => n.Y) + 1;
        var visited = new HashSet<(int, int)>();
        var queue = new Queue<(int x, int y, int steps)>();
        queue.Enqueue((emptyNode.X, emptyNode.Y, 0));
        visited.Add((emptyNode.X, emptyNode.Y));
        while (queue.Count > 0)
        {
            var (currentX, currentY, steps) = queue.Dequeue();
            var neighbors = GetNeighbors(currentX, currentY, gridWidth, gridHeight);
            foreach (var (nx, ny) in neighbors)
            {
                if (visited.Contains((nx, ny))) continue;
                var neighborNode = nodes.First(n => n.X == nx && n.Y == ny);
                if (neighborNode.Used > emptyNode.Size) continue;
                if ((nx == goalNode.X && ny == goalNode.Y - 1) ||
                    (nx == goalNode.X && ny == goalNode.Y + 1) ||
                    (nx == goalNode.X - 1 && ny == goalNode.Y) ||
                    (nx == goalNode.X + 1 && ny == goalNode.Y))
                {
                    var stepsToGoalAdjacency = steps + 1;
                    var stepsToMoveGoal = (Math.Abs(goalNode.X - 1) * 5) + 1;
                    return stepsToGoalAdjacency + stepsToMoveGoal;
                }

                visited.Add((nx, ny));
                queue.Enqueue((nx, ny, steps + 1));
            }
        }

        throw new Exception("Solution not found");
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
    /// Regex for parsing filesystem node entries in df format.
    /// </summary>
    [GeneratedRegex(@"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%")]
    private static partial Regex MyRegex();
}