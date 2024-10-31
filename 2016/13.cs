namespace _2016;

public abstract class _13
{
    private static readonly int Data;
    private static readonly (int x, int y)[] Directions = [(1, 0), (-1, 0), (0, 1), (0, -1)];

    static _13() => Data = int.Parse(Task.Run(() => Utils.GetInputData(13)).Result);

    private static bool IsWall(int x, int y)
    {
        if (x < 0 || y < 0) return true;

        var result = x * x + 3 * x + 2 * x * y + y + y * y;
        result += Data;
        var binary = Convert.ToString(result, 2);
        var bitCount = binary.Count(bit => bit == '1');
        return bitCount % 2 != 0;
    }

    private static int Bfs((int x, int y) start,
        Func<((int x, int y) position, int steps), bool> isEnd,
        Func<((int x, int y) position, int steps), bool> shouldContinue, out int visitedCount)
    {
        var queue = new Queue<((int x, int y) position, int steps)>();
        var visited = new HashSet<(int, int)>();

        queue.Enqueue((start, 0));
        visited.Add(start);

        while (queue.Count > 0)
        {
            var (currentPosition, steps) = queue.Dequeue();

            if (isEnd((currentPosition, steps)))
            {
                visitedCount = visited.Count;
                return steps;
            }

            if (!shouldContinue((currentPosition, steps))) continue;

            foreach (var direction in Directions)
            {
                var nextPosition = (x: currentPosition.x + direction.x, y: currentPosition.y + direction.y);
                if (visited.Contains(nextPosition) || IsWall(nextPosition.x, nextPosition.y)) continue;

                visited.Add(nextPosition);
                queue.Enqueue((nextPosition, steps + 1));
            }
        }

        visitedCount = visited.Count;
        return -1;
    }

    private static int SolvePartOne()
    {
        var start = (x: 1, y: 1);
        var destination = (x: 31, y: 39);
        return Bfs(start, endCondition => endCondition.position == destination, _ => true, out _);
    }

    private static int SolvePartTwo()
    {
        var start = (x: 1, y: 1);
        Bfs(start, _ => false, continueCondition => continueCondition.steps < 50, out var visitedCount);
        return visitedCount;
    }

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}