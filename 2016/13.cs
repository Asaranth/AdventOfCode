namespace _2016;

/// <summary>
/// Day 13: A Maze of Twisty Little Cubicles
/// 
/// Explores a mathematically generated infinite cubicle maze using breadth-first search and bit-parity wall detection.
/// </summary>
public abstract class _13
{
    private static readonly int Data;
    private static readonly (int x, int y)[] Directions = [(1, 0), (-1, 0), (0, 1), (0, -1)];

    static _13() => Data = int.Parse(Task.Run(() => Utils.GetInputData(13)).Result);

    /// <summary>
    /// Determines whether the coordinate (x, y) is a wall using polynomial coordinate arithmetic and bit parity.
    /// </summary>
    /// <param name="x">X coordinate (0-indexed column).</param>
    /// <param name="y">Y coordinate (0-indexed row).</param>
    /// <returns>True if the location is an impassable wall; otherwise, false.</returns>
    private static bool IsWall(int x, int y)
    {
        if (x < 0 || y < 0) return true;

        var result = x * x + 3 * x + 2 * x * y + y + y * y;
        result += Data;
        var binary = Convert.ToString(result, 2);
        var bitCount = binary.Count(bit => bit == '1');
        return bitCount % 2 != 0;
    }

    /// <summary>
    /// Executes a breadth-first search through the open spaces of the cubicle maze.
    /// </summary>
    /// <param name="start">Starting grid coordinates.</param>
    /// <param name="isEnd">Predicate to test if the goal position has been reached.</param>
    /// <param name="shouldContinue">Predicate to determine whether exploration should expand from the current step.</param>
    /// <param name="visitedCount">Outputs the total number of distinct locations visited during traversal.</param>
    /// <returns>The minimum steps taken to satisfy the end condition, or -1 if unreachable.</returns>
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

    /// <summary>
    /// Solves Part One: finds the fewest steps required to navigate from (1, 1) to (31, 39).
    /// </summary>
    /// <returns>Minimum step count to reach target.</returns>
    private static int SolvePartOne()
    {
        var start = (x: 1, y: 1);
        var destination = (x: 31, y: 39);
        return Bfs(start, endCondition => endCondition.position == destination, _ => true, out _);
    }

    /// <summary>
    /// Solves Part Two: counts the number of distinct locations reachable within at most 50 steps from (1, 1).
    /// </summary>
    /// <returns>Count of reachable cubicles in 50 steps.</returns>
    private static int SolvePartTwo()
    {
        var start = (x: 1, y: 1);
        Bfs(start, _ => false, continueCondition => continueCondition.steps < 50, out var visitedCount);
        return visitedCount;
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