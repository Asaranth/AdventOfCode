namespace _2016;

public abstract class _10
{
    private static readonly string[] Data;
    private static readonly Dictionary<int, (string lowType, int low, string highType, int high)> Instructions;
    private static readonly Dictionary<int, List<int>> BotChips;
    private static readonly Dictionary<int, int> Outputs;

    static _10()
    {
        Data = Task.Run(() => Utils.GetInputData(10)).Result.Split('\n', StringSplitOptions.RemoveEmptyEntries);
        Instructions = new Dictionary<int, (string lowType, int low, string highType, int high)>();
        BotChips = new Dictionary<int, List<int>>();
        Outputs = new Dictionary<int, int>();
    }

    private static void ParseInstructions()
    {
        var initialAssignments = new List<(int chip, int bot)>();

        foreach (var line in Data)
        {
            var parts = line.Split();
            switch (parts[0])
            {
                case "value":
                {
                    var chip = int.Parse(parts[1]);
                    var bot = int.Parse(parts[5]);
                    initialAssignments.Add((chip, bot));
                    break;
                }
                case "bot":
                {
                    var bot = int.Parse(parts[1]);
                    var lowType = parts[5];
                    var low = int.Parse(parts[6]);
                    var highType = parts[10];
                    var high = int.Parse(parts[11]);
                    Instructions[bot] = (lowType, low, highType, high);
                    break;
                }
            }
        }

        foreach (var (chip, bot) in initialAssignments)
        {
            if (!BotChips.ContainsKey(bot)) BotChips[bot] = [];
            BotChips[bot].Add(chip);
        }
    }

    private static void DistributeChip(string destType, int dest, int chip)
    {
        switch (destType)
        {
            case "bot":
                if (!BotChips.ContainsKey(dest)) BotChips[dest] = [];
                BotChips[dest].Add(chip);
                break;
            case "output":
                Outputs[dest] = chip;
                break;
        }
    }

    private static int SolvePartOne()
    {
        ParseInstructions();

        var targetBot = -1;
        var found = false;

        while (!found)
        {
            foreach (var bot in BotChips.Keys.Where(b => BotChips[b].Count == 2).ToList())
            {
                var chips = BotChips[bot];
                chips.Sort();

                if (chips[0] == 17 && chips[1] == 61)
                {
                    targetBot = bot;
                    found = true;
                    break;
                }

                var (lowType, low, highType, high) = Instructions[bot];
                DistributeChip(lowType, low, chips[0]);
                DistributeChip(highType, high, chips[1]);

                BotChips[bot].Clear();
            }
        }

        return targetBot;
    }

    private static int SolvePartTwo()
    {
        ParseInstructions();

        while (!Outputs.ContainsKey(0) || !Outputs.ContainsKey(1) || !Outputs.ContainsKey(2))
        {
            foreach (var bot in BotChips.Keys.Where(b => BotChips[b].Count == 2).ToList())
            {
                var chips = BotChips[bot];
                chips.Sort();

                var (lowType, low, highType, high) = Instructions[bot];
                DistributeChip(lowType, low, chips[0]);
                DistributeChip(highType, high, chips[1]);

                BotChips[bot].Clear();

                if (Outputs.ContainsKey(0) && Outputs.ContainsKey(1) && Outputs.ContainsKey(2)) break;
            }
        }

        return Outputs[0] * Outputs[1] * Outputs[2];
    }

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}