using System.Text.RegularExpressions;

namespace _2016;

/// <summary>
/// Day 11: Radioisotope Thermolectric Generators
/// 
/// Solves the RTG / microchip elevator transport puzzle using breadth-first search and bit-compressed state representation.
/// </summary>
public abstract partial class _11
{
    private static readonly string[] Data;
    private static HashSet<State> _seenStates = [];
    private static int _numberOfElements;

    static _11() => Data = Task.Run(() => Utils.GetInputData(11)).Result
        .Split('\n', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    /// Parses puzzle input text into an initial state with generator and microchip floor assignments.
    /// </summary>
    /// <param name="isPartTwo">If true, adds extra elerium and dilithium pairs on floor 0.</param>
    /// <returns>The initial starting state.</returns>
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

    /// <summary>
    /// Performs a breadth-first search across elevator and item movement states to find the minimum steps to reach the top floor.
    /// </summary>
    /// <param name="startingState">Initial system configuration.</param>
    /// <param name="getNextStates">Function generating valid successor states.</param>
    /// <returns>Minimum step count to assemble all items on the fourth floor.</returns>
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

    /// <summary>
    /// Generates and filters all valid successor states reachable from the current state.
    /// </summary>
    /// <param name="currentState">Current elevator and item state.</param>
    /// <returns>Collection of valid, unvisited successor states.</returns>
    private static IEnumerable<State> GenerateNextStates(State currentState)
    {
        var allStates = GenerateAllPossibleStates(currentState);
        return FilterValidStates(allStates);
    }

    /// <summary>
    /// Filters generated states to include only those satisfying safety constraints and not yet visited.
    /// </summary>
    /// <param name="allStates">Candidate states.</param>
    /// <returns>Filtered valid states.</returns>
    private static IEnumerable<State> FilterValidStates(IEnumerable<State> allStates) =>
        allStates.Distinct().Where(ValidateState).Where(s => !_seenStates.Contains(s));

    /// <summary>
    /// Validates radiation safety: a microchip cannot share a floor with an un-matched generator unless protected by its own generator.
    /// </summary>
    /// <param name="state">State to evaluate.</param>
    /// <returns>True if no microchip is fried; otherwise, false.</returns>
    private static bool ValidateState(State state)
    {
        var positions = state.DecompressPositions();
        return positions.All(p =>
            p.MicrochipFloor == p.GeneratorFloor || positions.All(p2 => p2.GeneratorFloor != p.MicrochipFloor));
    }

    /// <summary>
    /// Generates all possible move permutations to adjacent floors (up or down).
    /// </summary>
    /// <param name="currentState">Current configuration.</param>
    /// <returns>List of candidate states.</returns>
    private static List<State> GenerateAllPossibleStates(State currentState)
    {
        var nextStates = new List<State>();
        if (currentState.CurrentFloor != 0)
            nextStates.AddRange(GenerateStatesForFloor(currentState.CurrentFloor - 1, currentState));
        if (currentState.CurrentFloor != 3)
            nextStates.AddRange(GenerateStatesForFloor(currentState.CurrentFloor + 1, currentState));
        return nextStates;
    }

    /// <summary>
    /// Generates candidate states moving 1 or 2 items to the specified adjacent floor.
    /// </summary>
    /// <param name="nextFloor">Destination floor index.</param>
    /// <param name="currentState">Current configuration.</param>
    /// <returns>List of generated states.</returns>
    private static List<State> GenerateStatesForFloor(int nextFloor, State currentState)
    {
        var nextStates = new List<State>();
        nextStates.AddRange(GenerateSingleMoveStates(nextFloor, currentState));
        nextStates.AddRange(GenerateDoubleMoveStates(nextFloor, currentState));
        return nextStates;
    }

    /// <summary>
    /// Generates successor states by moving exactly one item located on the current floor.
    /// </summary>
    /// <param name="nextFloor">Destination floor index.</param>
    /// <param name="currentState">Current configuration.</param>
    /// <returns>List of single-item move states.</returns>
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

    /// <summary>
    /// Generates successor states by moving two items together from the current floor.
    /// </summary>
    /// <param name="nextFloor">Destination floor index.</param>
    /// <param name="currentState">Current configuration.</param>
    /// <returns>List of two-item move states.</returns>
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

    /// <summary>
    /// Checks whether all generators and microchips have reached the fourth floor (index 3).
    /// </summary>
    /// <param name="state">State to evaluate.</param>
    /// <returns>True if all items are on floor 3; otherwise, false.</returns>
    private static bool IsEndState(State state) =>
        state.DecompressPositions().All(t => t is { GeneratorFloor: 3, MicrochipFloor: 3 });

    /// <summary>
    /// Bit-packed representation of the current elevator floor and item positions.
    /// </summary>
    private record struct State
    {
        public int CurrentFloor;
        public int Positions;

        /// <summary>
        /// Decompresses the bit-packed integers into a list of generator and microchip positions.
        /// </summary>
        /// <returns>List of item positions.</returns>
        public IList<Position> DecompressPositions()
        {
            var ret = new List<Position>();

            for (var i = 0; i < _numberOfElements; i++)
                ret.Add(new Position { GeneratorFloor = GetFloorAt(2 * i), MicrochipFloor = GetFloorAt(2 * i + 1) });

            return ret;
        }

        /// <summary>
        /// Normalises and packs item floor positions into the integer bitfield.
        /// </summary>
        /// <param name="positions">Array of item positions.</param>
        public void SetPositions(Position[] positions)
        {
            Array.Sort(positions);

            for (var i = 0; i < _numberOfElements; i++)
            {
                SetFloorAt(2 * i, positions[i].GeneratorFloor);
                SetFloorAt(2 * i + 1, positions[i].MicrochipFloor);
            }
        }

        /// <summary>
        /// Reads a 2-bit floor value from the bitfield at the specified index.
        /// </summary>
        /// <param name="index">Item floor slot index.</param>
        /// <returns>Floor index (0–3).</returns>
        private int GetFloorAt(int index)
        {
            var mask = 3;
            mask <<= index * 2;
            var maskedPositions = Positions & mask;
            return maskedPositions >> (index * 2);
        }

        /// <summary>
        /// Writes a 2-bit floor value into the bitfield at the specified index.
        /// </summary>
        /// <param name="index">Item floor slot index.</param>
        /// <param name="floor">Floor index to set.</param>
        private void SetFloorAt(int index, int floor)
        {
            var floorInPosition = floor << (index * 2);
            var mask = 3;
            mask = ~(mask << (index * 2));
            Positions &= mask;
            Positions |= floorInPosition;
        }

        /// <summary>
        /// Creates a copy of this state.
        /// </summary>
        /// <returns>A new cloned State instance.</returns>
        public State CloneState() => new()
        {
            CurrentFloor = CurrentFloor,
            Positions = Positions
        };
    }

    /// <summary>
    /// Represents the floor positions of a single generator and its matching microchip.
    /// </summary>
    private struct Position : IComparable<Position>
    {
        public int GeneratorFloor;
        public int MicrochipFloor;

        int IComparable<Position>.CompareTo(Position other)
        {
            if (other.GeneratorFloor != GeneratorFloor) return other.GeneratorFloor - GeneratorFloor;
            return other.MicrochipFloor - MicrochipFloor;
        }

        /// <summary>
        /// Creates a clone of this position struct.
        /// </summary>
        /// <returns>Cloned Position copy.</returns>
        public Position Clone() => new()
        {
            GeneratorFloor = GeneratorFloor,
            MicrochipFloor = MicrochipFloor
        };
    }

    /// <summary>
    /// Solves Part One: finds minimum moves required to assemble the initial equipment on floor 4.
    /// </summary>
    /// <returns>Minimum move count for Part One.</returns>
    private static int SolvePartOne()
    {
        _seenStates = [];
        var problemInput = ParseInput();
        return BreadthFirstSearch(problemInput, GenerateNextStates);
    }

    /// <summary>
    /// Solves Part Two: finds minimum moves required when additional elerium and dilithium pairs are added.
    /// </summary>
    /// <returns>Minimum move count for Part Two.</returns>
    private static int SolvePartTwo()
    {
        _seenStates = [];
        var problemInput = ParseInput(isPartTwo: true);
        return BreadthFirstSearch(problemInput, GenerateNextStates);
    }

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    /// <summary>
    /// Regex for extracting generator element names.
    /// </summary>
    [GeneratedRegex(@"(\w+) generator")]
    private static partial Regex GeneratorRegex();

    /// <summary>
    /// Regex for extracting microchip element names.
    /// </summary>
    [GeneratedRegex(@"(\w+)-compatible microchip")]
    private static partial Regex MicrochipRegex();
}