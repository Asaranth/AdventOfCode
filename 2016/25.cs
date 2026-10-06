namespace _2016;

/// <summary>
/// Day 25: Clock Signal
/// 
/// Analyzes Assembunny bytecode execution to find the lowest positive initial integer generating an alternating binary clock signal.
/// </summary>
public static class _25
{
    private static readonly string[] Data;

    static _25() => Data = Task.Run(() => Utils.GetInputData(25)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Executes the bytecode for a test value and verifies whether the first 100 output values alternate between 0 and 1.
    /// </summary>
    /// <param name="initialValue">The candidate starting integer for register 'a'.</param>
    /// <returns>True if the output produces a 100-cycle alternating clock sequence; otherwise, false.</returns>
    private static bool TestSignal(int initialValue)
    {
        var registers = new Dictionary<string, int> { { "a", initialValue }, { "b", 0 }, { "c", 0 }, { "d", 0 } };
        var output = new List<int>();
        var currentIndex = 0;

        while (currentIndex < Data.Length && output.Count < 100)
        {
            var parts = Data[currentIndex].Split(' ');

            if (parts.Length < 2) continue;

            switch (parts[0])
            {
                case "cpy":
                    Cpy(parts, registers);
                    currentIndex++;
                    break;
                case "inc":
                    Inc(parts, registers);
                    currentIndex++;
                    break;
                case "dec":
                    Dec(parts, registers);
                    currentIndex++;
                    break;
                case "jnz":
                    currentIndex += Jnz(parts, registers);
                    break;
                case "out":
                    if (!Out(parts, registers, output)) return false;
                    currentIndex++;
                    break;
            }
        }

        return output.Count == 100;
    }

    /// <summary>
    /// Copies a literal or register value into the target register.
    /// </summary>
    /// <param name="parts">Instruction tokens.</param>
    /// <param name="registers">Dictionary of register states.</param>
    private static void Cpy(string[] parts, Dictionary<string, int> registers)
    {
        if (int.TryParse(parts[1], out var value)) registers[parts[2]] = value;
        else registers[parts[2]] = registers[parts[1]];
    }

    /// <summary>
    /// Increments the target register value.
    /// </summary>
    /// <param name="parts">Instruction tokens.</param>
    /// <param name="registers">Dictionary of register states.</param>
    private static void Inc(string[] parts, Dictionary<string, int> registers) => registers[parts[1]]++;

    /// <summary>
    /// Decrements the target register value.
    /// </summary>
    /// <param name="parts">Instruction tokens.</param>
    /// <param name="registers">Dictionary of register states.</param>
    private static void Dec(string[] parts, Dictionary<string, int> registers) => registers[parts[1]]--;

    /// <summary>
    /// Evaluates non-zero conditional jump.
    /// </summary>
    /// <param name="parts">Instruction tokens.</param>
    /// <param name="registers">Dictionary of register states.</param>
    /// <returns>Relative instruction pointer offset.</returns>
    private static int Jnz(string[] parts, Dictionary<string, int> registers)
    {
        var x = int.TryParse(parts[1], out var xValue) ? xValue : registers[parts[1]];
        var y = int.TryParse(parts[2], out var yValue) ? yValue : registers[parts[2]];
        return x != 0 ? y : 1;
    }

    /// <summary>
    /// Appends the register output to the signal list and validates that it alternates with the preceding value.
    /// </summary>
    /// <param name="parts">Instruction tokens.</param>
    /// <param name="registers">Dictionary of register states.</param>
    /// <param name="output">Accumulated output signal list.</param>
    /// <returns>True if the alternating clock signal remains valid; otherwise, false.</returns>
    private static bool Out(string[] parts, Dictionary<string, int> registers, List<int> output)
    {
        output.Add(registers[parts[1]]);
        return output.Count <= 1 || output[^1] == 1 - output[^2];
    }

    /// <summary>
    /// Finds the lowest positive integer for register 'a' that generates the infinite repeating clock signal.
    /// </summary>
    /// <returns>Lowest positive initial integer.</returns>
    private static int Solve()
    {
        for (var a = 1;; a++)
            if (TestSignal(a))
                return a;
    }

    /// <summary>
    /// Executes and prints the solution for the clock signal puzzle.
    /// </summary>
    public static void Run() => Console.WriteLine($"Solution: {Solve()}");
}