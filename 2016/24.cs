namespace _2016;

public static class _24
{
    private static readonly string[] Data;
    private static (int, int)[] _points;
    private static int[,] _distances;

    static _24() => Data = Task.Run(() => Utils.GetInputData(24)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

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

    private static int Solve(Func<int, int, int> computeEndCondition)
    {
        Initialize();
        var memo = new int[1 << _points.Length, _points.Length];
        for (var i = 0; i < memo.GetLength(0); i++)
        for (var j = 0; j < memo.GetLength(1); j++)
            memo[i, j] = -1;

        return Tsp(computeEndCondition, 1, 0, memo);
    }

    private static int SolvePartOne(int mask, int pos) => mask == (1 << _points.Length) - 1 ? 0 : -1;

    private static int SolvePartTwo(int mask, int pos) => mask == (1 << _points.Length) - 1 ? _distances[pos, 0] : -1;

    public static void Run()
    {
        Console.WriteLine($"Part One: {Solve(SolvePartOne)}");
        Console.WriteLine($"Part Two: {Solve(SolvePartTwo)}");
    }
}