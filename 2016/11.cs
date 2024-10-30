using System.Text.RegularExpressions;

namespace _2016;

public abstract partial class _11
{
    private static readonly string[] Data;
    private static HashSet<State> _seenStates = [];
    private static int _numberOfElements;

    static _11() => Data = Task.Run(() => Utils.GetInputData(11)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    private static State ParseInput(bool isPartTwo = false)
    {
        var elements = new Dictionary<string, Position>();

        for (var floor = 0; floor < Data.Length; floor++)
        {
            var line = Data[floor].ToLower();

            var generatorMatches = GeneratorRegex().Matches(line);
            var microchipMatches = MicrochipRegex().Matches(line);

            foreach (Match match in generatorMatches)
            {
                var elementName = match.Groups[1].Value;

                if (!elements.ContainsKey(elementName)) elements[elementName] = new Position();

                elements[elementName] = elements[elementName] with { GeneratorFloor = floor };
            }

            foreach (Match match in microchipMatches)
            {
                var elementName = match.Groups[1].Value;

                if (!elements.ContainsKey(elementName))
                    elements[elementName] = new Position();

                elements[elementName] = elements[elementName] with { MicrochipFloor = floor };
            }
        }

        var positions = elements.Values.OrderBy(p => p, Comparer<Position>.Default).ToArray();
        _numberOfElements = positions.Length;

        if (isPartTwo)
        {
            Array.Resize(ref positions, _numberOfElements + 2);
            positions[^2] = new Position { GeneratorFloor = 0, MicrochipFloor = 0 };
            positions[^1] = new Position { GeneratorFloor = 0, MicrochipFloor = 0 };
            _numberOfElements += 2;
        }

        var problemInput = new State
        {
            CurrentFloor = 0,
            Positions = 0
        };
        problemInput.SetPositions(positions);

        return problemInput;
    }

    private static int BreadthFirstSearch(State startingState, Func<State, IEnumerable<State>> getNextStates)
    {
        var nextStateQueue = new List<State> { startingState };
        _seenStates.Add(startingState);

        var currentDepth = 0;
        while (nextStateQueue.Count > 0)
        {
            var stateQueue = nextStateQueue;
            nextStateQueue = [];
            currentDepth++;

            foreach (var nextStates in stateQueue.Select(getNextStates))
            {
                var enumerable = nextStates.ToArray();

                if (enumerable.Any(IsEndState)) return currentDepth;

                foreach (var nextState in enumerable)
                {
                    _seenStates.Add(nextState);
                    nextStateQueue.Add(nextState);
                }
            }
        }

        return -1;
    }

    private static IEnumerable<State> GenerateNextStates(State currentState)
    {
        var allStates = GenerateAllPossibleStates(currentState);
        return FilterValidStates(allStates);
    }

    private static IEnumerable<State> FilterValidStates(IEnumerable<State> allStates) =>
        allStates.Distinct().Where(ValidateState).Where(s => !_seenStates.Contains(s));

    private static bool ValidateState(State state)
    {
        var positions = state.DecompressPositions();
        return positions.All(p =>
            p.MicrochipFloor == p.GeneratorFloor || positions.All(p2 => p2.GeneratorFloor != p.MicrochipFloor));
    }

    private static List<State> GenerateAllPossibleStates(State currentState)
    {
        var nextStates = new List<State>();
        if (currentState.CurrentFloor != 0)
            nextStates.AddRange(GenerateStatesForFloor(currentState.CurrentFloor - 1, currentState));
        if (currentState.CurrentFloor != 3)
            nextStates.AddRange(GenerateStatesForFloor(currentState.CurrentFloor + 1, currentState));
        return nextStates;
    }

    private static List<State> GenerateStatesForFloor(int nextFloor, State currentState)
    {
        var nextStates = new List<State>();
        nextStates.AddRange(GenerateSingleMoveStates(nextFloor, currentState));
        nextStates.AddRange(GenerateDoubleMoveStates(nextFloor, currentState));
        return nextStates;
    }

    private static List<State> GenerateSingleMoveStates(int nextFloor, State currentState)
    {
        var nextStates = new List<State>();
        var positions = currentState.DecompressPositions();

        for (var i = 0; i < positions.Count; i++)
        {
            if (positions[i].MicrochipFloor == currentState.CurrentFloor)
            {
                var nextState = currentState.CloneState();
                nextState.CurrentFloor = nextFloor;
                var nextPositions = positions.Select(p => p.Clone()).ToArray();
                nextPositions[i].MicrochipFloor = nextFloor;
                nextState.SetPositions(nextPositions);
                nextStates.Add(nextState);
            }

            if (positions[i].GeneratorFloor != currentState.CurrentFloor) continue;
            {
                var nextState = currentState.CloneState();
                nextState.CurrentFloor = nextFloor;
                var nextPositions = positions.Select(p => p.Clone()).ToArray();
                nextPositions[i].GeneratorFloor = nextFloor;
                nextState.SetPositions(nextPositions);
                nextStates.Add(nextState);
            }
        }

        return nextStates;
    }

    private static List<State> GenerateDoubleMoveStates(int nextFloor, State currentState)
    {
        var positions = currentState.DecompressPositions();
        var nextStates = new List<State>();

        for (var i = 0; i < positions.Count * 2; i++)
        {
            if ((i % 2 != 0 || positions[i / 2].GeneratorFloor != currentState.CurrentFloor) &&
                (i % 2 != 1 || positions[i / 2].MicrochipFloor != currentState.CurrentFloor)) continue;
            for (var j = i + 1; j < positions.Count * 2; j++)
            {
                if ((j % 2 != 0 || positions[j / 2].GeneratorFloor != currentState.CurrentFloor) &&
                    (j % 2 != 1 || positions[j / 2].MicrochipFloor != currentState.CurrentFloor)) continue;
                var nextState = currentState.CloneState();
                nextState.CurrentFloor = nextFloor;
                var nextPositions = positions.Select(p => p.Clone()).ToArray();

                if (i % 2 == 0) nextPositions[i / 2].GeneratorFloor = nextFloor;
                else nextPositions[i / 2].MicrochipFloor = nextFloor;
                if (j % 2 == 0) nextPositions[j / 2].GeneratorFloor = nextFloor;
                else nextPositions[j / 2].MicrochipFloor = nextFloor;

                nextState.SetPositions(nextPositions);
                nextStates.Add(nextState);
            }
        }

        return nextStates;
    }

    private static bool IsEndState(State state) =>
        state.DecompressPositions().All(t => t is { GeneratorFloor: 3, MicrochipFloor: 3 });

    private record struct State
    {
        public int CurrentFloor;
        public int Positions;

        public IList<Position> DecompressPositions()
        {
            var ret = new List<Position>();

            for (var i = 0; i < _numberOfElements; i++)
                ret.Add(new Position { GeneratorFloor = GetFloorAt(2 * i), MicrochipFloor = GetFloorAt(2 * i + 1) });

            return ret;
        }

        public void SetPositions(Position[] positions)
        {
            Array.Sort(positions);

            for (var i = 0; i < _numberOfElements; i++)
            {
                SetFloorAt(2 * i, positions[i].GeneratorFloor);
                SetFloorAt(2 * i + 1, positions[i].MicrochipFloor);
            }
        }

        private int GetFloorAt(int index)
        {
            var mask = 3;
            mask <<= index * 2;
            var maskedPositions = Positions & mask;
            return maskedPositions >> (index * 2);
        }

        private void SetFloorAt(int index, int floor)
        {
            var floorInPosition = floor << (index * 2);
            var mask = 3;
            mask = ~(mask << (index * 2));
            Positions &= mask;
            Positions |= floorInPosition;
        }

        public State CloneState() => new()
        {
            CurrentFloor = CurrentFloor,
            Positions = Positions
        };
    }

    private struct Position : IComparable<Position>
    {
        public int GeneratorFloor;
        public int MicrochipFloor;

        int IComparable<Position>.CompareTo(Position other)
        {
            if (other.GeneratorFloor != GeneratorFloor) return other.GeneratorFloor - GeneratorFloor;
            return other.MicrochipFloor - MicrochipFloor;
        }

        public Position Clone() => new()
        {
            GeneratorFloor = GeneratorFloor,
            MicrochipFloor = MicrochipFloor
        };
    }

    private static int SolvePartOne()
    {
        _seenStates = [];
        var problemInput = ParseInput();
        return BreadthFirstSearch(problemInput, GenerateNextStates);
    }

    private static int SolvePartTwo()
    {
        _seenStates = [];
        var problemInput = ParseInput(isPartTwo: true);
        return BreadthFirstSearch(problemInput, GenerateNextStates);
    }

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    [GeneratedRegex(@"(\w+) generator")]
    private static partial Regex GeneratorRegex();

    [GeneratedRegex(@"(\w+)-compatible microchip")]
    private static partial Regex MicrochipRegex();
}