namespace _2016;

public static class _23
{
    private static readonly string[] Data;

    static _23() => Data = Task.Run(() => Utils.GetInputData(23)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

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

    private static void Cpy(string x, string y, Dictionary<string, int> registers)
    {
        if (int.TryParse(y, out _)) return;
        registers[y] = int.TryParse(x, out var value) ? value : registers[x];
    }

    private static void Inc(string x, Dictionary<string, int> registers)
    {
        if (registers.TryGetValue(x, out var value)) registers[x] = ++value;
    }

    private static void Dec(string x, Dictionary<string, int> registers)
    {
        if (registers.TryGetValue(x, out var value)) registers[x] = --value;
    }

    private static int Jnz(string x, string y, Dictionary<string, int> registers)
    {
        var xValue = int.TryParse(x, out var valueX) ? valueX : registers[x];
        var yValue = int.TryParse(y, out var valueY) ? valueY : registers[y];
        return xValue != 0 ? yValue : 1;
    }

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

    private static Dictionary<string, int> CreateInitialRegisters(int eggs) =>
        new() { { "a", eggs }, { "b", 0 }, { "c", 0 }, { "d", 0 } };

    private static int SolvePartOne()
    {
        var registers = CreateInitialRegisters(7);
        ExecuteProgram((string[])Data.Clone(), registers);
        return registers["a"];
    }

    private static int SolvePartTwo()
    {
        var registers = CreateInitialRegisters(12);
        ExecuteProgram((string[])Data.Clone(), registers);
        return registers["a"];
    }

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}