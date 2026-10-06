namespace _2016;

/// <summary>
/// Day 12: Leonardo's Monorail
/// 
/// Executes Assembunny bytecode instructions manipulating register values and unconditional/conditional jumps.
/// </summary>
public abstract class _12
{
    private static readonly string[] Data;

    static _12() => Data = Task.Run(() => Utils.GetInputData(12)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Executes the Assembunny instructions with the given initial register states until the pointer leaves the program.
    /// </summary>
    /// <param name="registers">Dictionary mapping register names to current integer values.</param>
    /// <returns>The final value stored in register 'a'.</returns>
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

    /// <summary>
    /// Solves Part One: executes the program with all registers initialised to 0.
    /// </summary>
    /// <returns>Value of register 'a' for Part One.</returns>
    private static int SolvePartOne() =>
        ExecuteInstructions(new Dictionary<string, int> { { "a", 0 }, { "b", 0 }, { "c", 0 }, { "d", 0 } });

    /// <summary>
    /// Solves Part Two: executes the program with register 'c' initialised to 1.
    /// </summary>
    /// <returns>Value of register 'a' for Part Two.</returns>
    private static int SolvePartTwo() =>
        ExecuteInstructions(new Dictionary<string, int> { { "a", 0 }, { "b", 0 }, { "c", 1 }, { "d", 0 } });

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}