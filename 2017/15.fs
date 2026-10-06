namespace _2017

open System

/// <summary>
/// Day 15: Dueling Generators
///
/// Generates pseudo-random number sequences with modular multiplication, comparing the lowest 16 bits of matching pairs.
/// </summary>
module _15 =
    let Data = (Utils.GetInputData 15).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Parses the starting seed value for a specified generator from input text.
    /// </summary>
    /// <param name="prefix">Generator identifier prefix line.</param>
    /// <returns>Starting seed integer value.</returns>
    let parseData(prefix: string) = Data |> Array.find (_.StartsWith(prefix)) |> fun line -> line.Split(' ').[4] |> int64

    /// <summary>
    /// Computes the next generator state using standard Lehmer random number multiplier and 2147483647 modulus.
    /// </summary>
    /// <param name="previousValue">Previous state integer.</param>
    /// <param name="factor">Multiplication factor.</param>
    /// <returns>Next pseudo-random value.</returns>
    let generateNextValue (previousValue, factor) = (previousValue * factor) % 2147483647L

    /// <summary>
    /// Extracts the lowest 16 bits of an integer value.
    /// </summary>
    /// <param name="value">The 64-bit integer.</param>
    /// <returns>Lowest 16 bits as an integer mask.</returns>
    let lowest16Bits value = value &&& 0xFFFFL

    /// <summary>
    /// Simulates generator iterations and counts matching lowest 16-bit pairs that meet specific divisibility criteria.
    /// </summary>
    /// <param name="iterations">Number of valid pairs to test.</param>
    /// <param name="filterA">Predicate function to accept values from Generator A.</param>
    /// <param name="filterB">Predicate function to accept values from Generator B.</param>
    /// <returns>Count of matching 16-bit pairs.</returns>
    let countMatchingPairs iterations filterA filterB =
        let mutable count = 0
        let mutable valueA = parseData "Generator A starts with"
        let mutable valueB = parseData "Generator B starts with"
        let mutable validPairs = 0
        while validPairs < iterations do
            valueA <- generateNextValue (valueA, 16807L)
            if filterA valueA then
                valueB <- generateNextValue (valueB, 48271L)
                while not (filterB valueB) do valueB <- generateNextValue (valueB, 48271L)
                if (lowest16Bits valueA) = (lowest16Bits valueB) then count <- count + 1
                validPairs <- validPairs + 1
        count

    /// <summary>
    /// Solves Part 1: counts 16-bit matches over 40 million unfiltered iterations.
    /// </summary>
    /// <returns>Matching pair count for Part 1.</returns>
    let solvePartOne() = countMatchingPairs 40000000 (fun _ -> true) (fun _ -> true)

    /// <summary>
    /// Solves Part 2: counts 16-bit matches over 5 million filtered pairs (multiples of 4 for A and multiples of 8 for B).
    /// </summary>
    /// <returns>Matching pair count for Part 2.</returns>
    let solvePartTwo() = countMatchingPairs 5000000 (fun value -> value % 4L = 0L) (fun value -> value % 8L = 0L)

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"