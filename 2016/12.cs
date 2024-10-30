namespace _2016;

public abstract class _12
{
    private static readonly string[] Data;

    static _12() => Data = Task.Run(() => Utils.GetInputData(12)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    private static int ExecuteInstructions(Dictionary<string, int> registers)
    {
        var instructions = Data;
        var pointer = 0;

        while (pointer < instructions.Length)
        {
            var parts = instructions[pointer].Split(' ');

            switch (parts[0])
            {
                case "cpy":
                    if (int.TryParse(parts[1], out var value)) registers[parts[2]] = value;
                    else registers[parts[2]] = registers[parts[1]];
                    pointer++;
                    break;
                case "inc":
                    registers[parts[1]]++;
                    pointer++;
                    break;
                case "dec":
                    registers[parts[1]]--;
                    pointer++;
                    break;
                case "jnz":
                    if (int.TryParse(parts[1], out var cmpValue))
                        if (cmpValue != 0) pointer += int.Parse(parts[2]);
                        else pointer++;
                    else if (registers[parts[1]] != 0) pointer += int.Parse(parts[2]);
                        else pointer++;
                    break;
                default:
                    throw new InvalidOperationException($"Unknown instruction {parts[0]}");
            }
        }

        return registers["a"];
    }

    private static int SolvePartOne() =>
        ExecuteInstructions(new Dictionary<string, int> { { "a", 0 }, { "b", 0 }, { "c", 0 }, { "d", 0 } });

    private static int SolvePartTwo() =>
        ExecuteInstructions(new Dictionary<string, int> { { "a", 0 }, { "b", 0 }, { "c", 1 }, { "d", 0 } });

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}