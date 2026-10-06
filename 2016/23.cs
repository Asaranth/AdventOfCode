namespace _2016;

/// <summary>
/// Day 23: Safe Cracking
/// 
/// Executes Assembunny code with self-modifying toggle (tgl) instructions and multiplication loop optimisation.
/// </summary>
public static class _23
{
    private static readonly string[] Data;

    static _23() => Data = Task.Run(() => Utils.GetInputData(23)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Executes the self-modifying Assembunny instruction list until the instruction pointer exceeds the program length.
    /// </summary>
    /// <param name="instructions">Mutable array of instruction strings.</param>
    /// <param name="registers">Dictionary containing current register values.</param>
    private static void ExecuteProgram(string[] instructions, Dictionary<string, int> registers)
    {
        for (var i = 0; i < instructions.Length;)
        {
            var instructionParts = instructions[i].Split(' ');

            if (TryOptimizeMultiplicationPattern(instructions, registers, ref i)) continue;

            switch (instructionParts[0])
            {
                case "cpy":
                    Cpy(instructionParts[1], instructionParts[2], registers);
                    break;
                case "inc":
                    Inc(instructionParts[1], registers);
                    break;
                case "dec":
                    Dec(instructionParts[1], registers);
                    break;
                case "jnz":
                    i += Jnz(instructionParts[1], instructionParts[2], registers);
                    continue;
                case "tgl":
                    Tgl(instructionParts[1], i, instructions, registers);
                    break;
            }

            i++;
        }
    }

    /// <summary>
    /// Copies value or source register content into the destination register (ignores invalid writes to literal values).
    /// </summary>
    /// <param name="x">Source value or register.</param>
    /// <param name="y">Target register.</param>
    /// <param name="registers">Dictionary of register states.</param>
    private static void Cpy(string x, string y, Dictionary<string, int> registers)
    {
        if (int.TryParse(y, out _)) return;
        registers[y] = int.TryParse(x, out var value) ? value : registers[x];
    }

    /// <summary>
    /// Increments the value of the specified register.
    /// </summary>
    /// <param name="x">Target register.</param>
    /// <param name="registers">Dictionary of register states.</param>
    private static void Inc(string x, Dictionary<string, int> registers)
    {
        if (registers.TryGetValue(x, out var value)) registers[x] = ++value;
    }

    /// <summary>
    /// Decrements the value of the specified register.
    /// </summary>
    /// <param name="x">Target register.</param>
    /// <param name="registers">Dictionary of register states.</param>
    private static void Dec(string x, Dictionary<string, int> registers)
    {
        if (registers.TryGetValue(x, out var value)) registers[x] = --value;
    }

    /// <summary>
    /// Computes the relative instruction jump offset when the test condition register or value is non-zero.
    /// </summary>
    /// <param name="x">Condition register or literal integer.</param>
    /// <param name="y">Jump offset register or literal integer.</param>
    /// <param name="registers">Dictionary of register states.</param>
    /// <returns>Relative jump offset, or 1 to proceed to the next instruction.</returns>
    private static int Jnz(string x, string y, Dictionary<string, int> registers)
    {
        var xValue = int.TryParse(x, out var valueX) ? valueX : registers[x];
        var yValue = int.TryParse(y, out var valueY) ? valueY : registers[y];
        return xValue != 0 ? yValue : 1;
    }

    /// <summary>
    /// Toggles the instruction at an offset relative to the current instruction pointer.
    /// </summary>
    /// <param name="x">Offset register or integer value.</param>
    /// <param name="currentIndex">Current instruction pointer position.</param>
    /// <param name="instructions">Array of program instructions modified in-place.</param>
    /// <param name="registers">Dictionary of register states.</param>
    private static void Tgl(string x, int currentIndex, string[] instructions, Dictionary<string, int> registers)
    {
        var xValue = int.TryParse(x, out var value) ? value : registers[x];
        var targetIndex = currentIndex + xValue;

        if (targetIndex < 0 || targetIndex >= instructions.Length) return;

        var targetInstruction = instructions[targetIndex].Split(' ');

        instructions[targetIndex] = targetInstruction.Length switch
        {
            2 => targetInstruction[0] == "inc"
                ? $"dec {targetInstruction[1]}"
                : $"inc {targetInstruction[1]}",
            3 => targetInstruction[0] == "jnz"
                ? $"cpy {targetInstruction[1]} {targetInstruction[2]}"
                : $"jnz {targetInstruction[1]} {targetInstruction[2]}",
            _ => instructions[targetIndex]
        };
    }

    /// <summary>
    /// Detects and fast-forwards repeated nested addition loops that perform multiplication (a += b * d).
    /// </summary>
    /// <param name="instructions">Array of instructions.</param>
    /// <param name="registers">Dictionary of register states.</param>
    /// <param name="index">Current instruction pointer reference.</param>
    /// <returns>True if the multiplication pattern was matched and executed; otherwise, false.</returns>
    private static bool TryOptimizeMultiplicationPattern(string[] instructions, Dictionary<string, int> registers, ref int index)
    {
        if (index + 5 >= instructions.Length
            || instructions[index] != "cpy b c"
            || instructions[index + 1] != "inc a"
            || instructions[index + 2] != "dec c"
            || instructions[index + 3] != "jnz c -2"
            || instructions[index + 4] != "dec d"
            || instructions[index + 5] != "jnz d -5")
            return false;

        registers["a"] += registers["b"] * registers["d"];
        registers["c"] = 0;
        registers["d"] = 0;
        index += 6;

        return true;
    }

    /// <summary>
    /// Initialises registers for execution with a specified initial egg count in register 'a'.
    /// </summary>
    /// <param name="eggs">Initial integer value for register 'a'.</param>
    /// <returns>Initialised register dictionary.</returns>
    private static Dictionary<string, int> CreateInitialRegisters(int eggs) =>
        new() { { "a", eggs }, { "b", 0 }, { "c", 0 }, { "d", 0 } };

    /// <summary>
    /// Solves Part One: runs the safe-cracking bytecode with 7 eggs.
    /// </summary>
    /// <returns>Final value of register 'a' for Part One.</returns>
    private static int SolvePartOne()
    {
        var registers = CreateInitialRegisters(7);
        ExecuteProgram((string[])Data.Clone(), registers);
        return registers["a"];
    }

    /// <summary>
    /// Solves Part Two: runs the safe-cracking bytecode with 12 eggs.
    /// </summary>
    /// <returns>Final value of register 'a' for Part Two.</returns>
    private static int SolvePartTwo()
    {
        var registers = CreateInitialRegisters(12);
        ExecuteProgram((string[])Data.Clone(), registers);
        return registers["a"];
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