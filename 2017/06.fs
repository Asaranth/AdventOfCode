namespace _2017

open System
open System.Collections.Generic;

/// <summary>
/// Day 06: Memory Reallocation
///
/// Simulates cyclical memory bank reallocation routines to detect infinite loops and calculate loop sizes.
/// </summary>
module _06 =
    let Data = (Utils.GetInputData 6).Split('\t', StringSplitOptions.RemoveEmptyEntries) |> Array.map int

    /// <summary>
    /// Reallocates memory blocks from the bank with the maximum blocks cyclically across all banks in place.
    /// </summary>
    /// <param name="banks">Array of memory bank block counts.</param>
    let redistribute(banks: int[]) =
        let len = banks.Length
        let maxBlocks = Array.max banks
        let index = Array.findIndex(fun i -> i = maxBlocks) banks
        banks[index] <- 0
        for i in 1 .. maxBlocks do
            banks[(index + i) % len] <- banks[(index + i) % len] + 1

    /// <summary>
    /// Solves Part 1: counts reallocation cycles completed before a configuration repeats.
    /// </summary>
    /// <returns>Number of reallocation cycles until a state repeat.</returns>
    let solvePartOne() =
        let seenConfigurations = HashSet<string>()
        let rec distribute cycles =
            let config = String.Join(',', Data)
            if seenConfigurations.Contains(config) then cycles
            else
                seenConfigurations.Add(config) |> ignore
                redistribute Data
                distribute (cycles + 1)
        distribute 0

    /// <summary>
    /// Solves Part 2: determines the size of the loop between repetitions of the recurring configuration.
    /// </summary>
    /// <returns>Number of cycles in the infinite loop.</returns>
    let solvePartTwo() =
        let seenConfigurations = Dictionary<string, int>()
        let rec distribute (banks: int[]) cycles =
            let config = String.Join(',', banks)
            if seenConfigurations.ContainsKey(config) then
                cycles - seenConfigurations[config]
            else
                seenConfigurations[config] <- cycles
                redistribute banks
                distribute banks (cycles + 1)
        distribute (Array.copy Data) 0

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"