namespace _2016;

public static class _25
{
    private static readonly string[] Data;

    static _25() => Data = Task.Run(() => Utils.GetInputData(25)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

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

    private static void Cpy(string[] parts, Dictionary<string, int> registers)
    {
        if (int.TryParse(parts[1], out var value)) registers[parts[2]] = value;
        else registers[parts[2]] = registers[parts[1]];
    }

    private static void Inc(string[] parts, Dictionary<string, int> registers) => registers[parts[1]]++;

    private static void Dec(string[] parts, Dictionary<string, int> registers) => registers[parts[1]]--;

    private static int Jnz(string[] parts, Dictionary<string, int> registers)
    {
        var x = int.TryParse(parts[1], out var xValue) ? xValue : registers[parts[1]];
        var y = int.TryParse(parts[2], out var yValue) ? yValue : registers[parts[2]];
        return x != 0 ? y : 1;
    }

    private static bool Out(string[] parts, Dictionary<string, int> registers, List<int> output)
    {
        output.Add(registers[parts[1]]);
        return output.Count <= 1 || output[^1] == 1 - output[^2];
    }

    private static int Solve()
    {
        for (var a = 1;; a++)
            if (TestSignal(a))
                return a;
    }

    public static void Run() => Console.WriteLine($"Solution: {Solve()}");
}