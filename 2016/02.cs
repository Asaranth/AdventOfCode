namespace _2016;

/// <summary>
/// Day 02: Bathroom Security
/// 
/// Decodes bathroom entry codes by traversing standard 3x3 and diamond-shaped keypads.
/// </summary>
public abstract class _02
{
    private static readonly string[] Data;

    static _02() => Data = Task.Run(() => Utils.GetInputData(2)).Result
        .Split(['\r', '\n'], StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Solves Part One: determines the bathroom code on a standard 3x3 numeric keypad.
    /// </summary>
    /// <returns>The decoded numeric code as an integer.</returns>
    private static int SolvePartOne()
    {
        int[,] keypad = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };
        int x = 1, y = 1;
        var code = new List<int>();

        foreach (var line in Data)
        {
            foreach (var move in line)
            {
                switch (move)
                {
                    case 'U':
                        x = Math.Max(0, x - 1);
                        break;
                    case 'D':
                        x = Math.Min(2, x + 1);
                        break;
                    case 'L':
                        y = Math.Max(0, y - 1);
                        break;
                    case 'R':
                        y = Math.Min(2, y + 1);
                        break;
                }
            }

            code.Add(keypad[x, y]);
        }

        return int.Parse(string.Join("", code));
    }

    /// <summary>
    /// Solves Part Two: determines the bathroom code on an actual diamond-shaped alphanumeric keypad.
    /// </summary>
    /// <returns>The decoded bathroom code string.</returns>
    private static string SolvePartTwo()
    {
        var instructions = Data;
        var keypad = new Dictionary<(int, int), char>
        {
            { (0, 2), '1' },
            { (1, 1), '2' }, { (1, 2), '3' }, { (1, 3), '4' },
            { (2, 0), '5' }, { (2, 1), '6' }, { (2, 2), '7' }, { (2, 3), '8' }, { (2, 4), '9' },
            { (3, 1), 'A' }, { (3, 2), 'B' }, { (3, 3), 'C' },
            { (4, 2), 'D' }
        };

        var currentPos = (x: 2, y: 0);
        var code = new List<char>();

        foreach (var line in instructions)
        {
            foreach (var nextPos in line.Select(move => move switch
                     {
                         'U' => (currentPos.x - 1, currentPos.y),
                         'D' => (currentPos.x + 1, currentPos.y),
                         'L' => (currentPos.x, currentPos.y - 1),
                         'R' => (currentPos.x, currentPos.y + 1),
                         _ => currentPos
                     }).Where(nextPos => keypad.ContainsKey(nextPos))) currentPos = nextPos;

            code.Add(keypad[currentPos]);
        }

        return string.Join("", code);
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