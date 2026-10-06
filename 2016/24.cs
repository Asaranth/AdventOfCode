namespace _2016;

/// <summary>
/// Day 24: Air Duct Spelunking
/// 
/// Solves the Traveling Salesperson Problem over duct maze points of interest using BFS all-pairs shortest paths and dynamic programming with bitmask memoisation.
/// </summary>
public static class _24
{
    private static readonly string[] Data;
    private static (int, int)[] _points = [];
    private static int[,] _distances = new int[0 ,0];

    static _24() => Data = Task.Run(() => Utils.GetInputData(24)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Identifies all numbered points of interest in the maze and precomputes pairwise shortest distances.
    /// </summary>
    private static void Initialize()
    {
        var pointsList = new List<(int, int)>();
        for (var r = 0; r < Data.Length; r++)
        for (var c = 0; c < Data[r].Length; c++)
            if (char.IsDigit(Data[r][c]))
                pointsList.Add((r, c));

        _points = pointsList.OrderBy(p => Data[p.Item1][p.Item2]).ToArray();
        _distances = new int[_points.Length, _points.Length];
        CalculateDistances();
    }

    /// <summary>
    /// Performs BFS from each numbered point to populate the all-pairs shortest distance matrix.
    /// </summary>
    private static void CalculateDistances()
    {
        for (var i = 0; i < _points.Length; i++)
        {
            var queue = new Queue<(int r, int c, int dist)>();
            var visited = new HashSet<(int, int)>();
            queue.Enqueue((_points[i].Item1, _points[i].Item2, 0));
            visited.Add(_points[i]);

            while (queue.Count != 0)
            {
                var (r, c, dist) = queue.Dequeue();
                var directions = new[] { (0, 1), (1, 0), (0, -1), (-1, 0) };

                foreach (var (dr, dc) in directions)
                {
                    int nr = r + dr, nc = c + dc;
                    if (nr < 0 || nr >= Data.Length || nc < 0 || nc >= Data[0].Length ||
                        Data[nr][nc] == '#' || !visited.Add((nr, nc))) continue;
                    queue.Enqueue((nr, nc, dist + 1));
                    var pointIndex = Array.IndexOf(_points, (nr, nc));
                    if (char.IsDigit(Data[nr][nc])) _distances[i, pointIndex] = dist + 1;
                }
            }
        }
    }

    /// <summary>
    /// Recursively computes the minimum distance to visit all remaining points using bitmask dynamic programming.
    /// </summary>
    /// <param name="computeEndCondition">Function computing terminal cost once all locations have been visited.</param>
    /// <param name="mask">Bitmask of visited points.</param>
    /// <param name="pos">Current point index.</param>
    /// <param name="memo">Memoisation table.</param>
    /// <returns>Minimum total travel distance.</returns>
    private static int Tsp(Func<int, int, int> computeEndCondition, int mask, int pos, int[,] memo)
    {
        if (computeEndCondition(mask, pos) != -1) return computeEndCondition(mask, pos);

        if (memo[mask, pos] != -1) return memo[mask, pos];

        var res = int.MaxValue;
        for (var city = 0; city < _points.Length; city++)
        {
            if ((mask & (1 << city)) != 0) continue;

            var newRes = _distances[pos, city] + Tsp(computeEndCondition, mask | (1 << city), city, memo);
            res = Math.Min(res, newRes);
        }

        return memo[mask, pos] = res;
    }

    /// <summary>
    /// Initialises matrices and executes TSP starting from point '0'.
    /// </summary>
    /// <param name="computeEndCondition">Terminal condition handler.</param>
    /// <returns>Shortest path length.</returns>
    private static int Solve(Func<int, int, int> computeEndCondition)
    {
        Initialize();
        var memo = new int[1 << _points.Length, _points.Length];
        for (var i = 0; i < memo.GetLength(0); i++)
        for (var j = 0; j < memo.GetLength(1); j++)
            memo[i, j] = -1;

        return Tsp(computeEndCondition, 1, 0, memo);
    }

    /// <summary>
    /// Evaluates the end condition for Part One: visiting all points with no return to origin required.
    /// </summary>
    /// <param name="mask">Current visited bitmask.</param>
    /// <param name="pos">Current point index.</param>
    /// <returns>0 if complete; -1 otherwise.</returns>
    private static int SolvePartOne(int mask, int pos) => mask == (1 << _points.Length) - 1 ? 0 : -1;

    /// <summary>
    /// Evaluates the end condition for Part Two: visiting all points and returning back to starting location '0'.
    /// </summary>
    /// <param name="mask">Current visited bitmask.</param>
    /// <param name="pos">Current point index.</param>
    /// <returns>Distance back to point 0 if complete; -1 otherwise.</returns>
    private static int SolvePartTwo(int mask, int pos) => mask == (1 << _points.Length) - 1 ? _distances[pos, 0] : -1;

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {Solve(SolvePartOne)}");
        Console.WriteLine($"Part Two: {Solve(SolvePartTwo)}");
    }
}