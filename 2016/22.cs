using System.Text.RegularExpressions;

namespace _2016;

public static partial class _22
{
    private static readonly string[] Data;

    static _22() => Data = Task.Run(() => Utils.GetInputData(22)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

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

    private static IEnumerable<(int x, int y)> GetNeighbors(int x, int y, int width, int height)
    {
        if (x > 0) yield return (x - 1, y);
        if (x < width - 1) yield return (x + 1, y);
        if (y > 0) yield return (x, y - 1);
        if (y < height - 1) yield return (x, y + 1);
    }

    private class Node(int x, int y, int size, int used, int avail)
    {
        public int X { get; } = x;
        public int Y { get; } = y;
        public int Size { get; } = size;
        public int Used { get; } = used;
        public int Avail { get; } = avail;
    }

    private static int SolvePartOne()
    {
        var nodes = ParseNodes();
        return nodes.Select((s, i) => nodes.Where((t, j) => i != j && s.Used > 0 && s.Used <= t.Avail).Count()).Sum();
    }

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

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    [GeneratedRegex(@"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%")]
    private static partial Regex MyRegex();
}