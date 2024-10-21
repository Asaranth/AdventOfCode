namespace _2016
{
    public abstract class _03
    {
        private static readonly string[] Data;

        static _03() => Data = Task.Run(() => Utils.GetInputData(3)).Result
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);

        private static bool ValidTriangle(int[] sides) =>
            sides[0] + sides[1] > sides[2] && sides[1] + sides[2] > sides[0] && sides[2] + sides[0] > sides[1];

        private static int SolvePartOne() =>
            Data.Select(line =>
                    line.Trim().Split([' '], StringSplitOptions.RemoveEmptyEntries).Select(int.Parse).ToArray())
                .Count(ValidTriangle);

        private static int SolvePartTwo()
        {
            var validTriangleCount = 0;
            var columns = new List<List<int>> { new(), new(), new() };

            for (var row = 0; row < Data.Length; row += 3)
            {
                var firstRow = Data[row].Trim().Split([' '], StringSplitOptions.RemoveEmptyEntries)
                    .Select(int.Parse).ToArray();
                var secondRow = Data[row + 1].Trim().Split([' '], StringSplitOptions.RemoveEmptyEntries)
                    .Select(int.Parse).ToArray();
                var thirdRow = Data[row + 2].Trim().Split([' '], StringSplitOptions.RemoveEmptyEntries)
                    .Select(int.Parse).ToArray();

                for (var col = 0; col < 3; col++)
                {
                    columns[col].Add(firstRow[col]);
                    columns[col].Add(secondRow[col]);
                    columns[col].Add(thirdRow[col]);
                }
            }

            foreach (var col in columns)
            {
                for (var i = 0; i < col.Count; i += 3)
                {
                    var sides = new[] { col[i], col[i + 1], col[i + 2] };
                    if (ValidTriangle(sides)) validTriangleCount++;
                }
            }

            return validTriangleCount;
        }

        public static void Run()
        {
            Console.WriteLine($"Part One: {SolvePartOne()}");
            Console.WriteLine($"Part Two: {SolvePartTwo()}");
        }
    }
}