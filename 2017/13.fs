namespace _2017

open System

/// <summary>
/// Day 13: Packet Scanners
///
/// Simulates firewall scanner round-trip oscillations and calculates traversal severity and collision-free delays.
/// </summary>
module _13 =
    let Data = (Utils.GetInputData 13).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Parses puzzle input lines into pairs of (layer depth, scanner range).
    /// </summary>
    let parseInput = Data |> Array.map(fun line ->
        let parts = line.Split(':', StringSplitOptions.RemoveEmptyEntries)
        Int32.Parse(parts[0]), Int32.Parse(parts[1].Trim()))

    /// <summary>
    /// Computes trip severity if departing at picosecond 0 by summing (depth * range) for all layers where the scanner is at index 0.
    /// </summary>
    /// <param name="firewall">Array of (depth, range) scanner specifications.</param>
    /// <returns>Total trip severity score.</returns>
    let calculateSeverity firewall =
        firewall |> Array.fold(fun severity (depth, range) ->
            let cycle = (range - 1) * 2
            if depth % cycle = 0 then severity + (depth * range)
            else severity) 0

    /// <summary>
    /// Checks if a packet starting with a given delay gets caught by any firewall scanner.
    /// </summary>
    /// <param name="delay">Delay in picoseconds before starting the trip.</param>
    /// <param name="firewall">Array of (depth, range) scanner specifications.</param>
    /// <returns>True if caught by at least one scanner; false otherwise.</returns>
    let isCaught delay firewall =
        firewall |> Array.exists(fun (depth, range) ->
            let cycle = (range - 1) * 2
            (depth + delay) % cycle = 0)

    /// <summary>
    /// Finds the minimum delay required to pass through the entire firewall without being caught by any scanner.
    /// </summary>
    /// <param name="firewall">Array of (depth, range) scanner specifications.</param>
    /// <returns>Smallest non-negative delay in picoseconds.</returns>
    let findDelay firewall =
        let rec search delay =
            if not (isCaught delay firewall) then delay
            else search (delay + 1)
        search 0

    /// <summary>
    /// Solves Part 1: calculates severity of traveling through the firewall starting at picosecond 0.
    /// </summary>
    /// <returns>Trip severity for Part 1.</returns>
    let solvePartOne() = calculateSeverity parseInput

    /// <summary>
    /// Solves Part 2: finds the fewest picoseconds to delay departure to pass through undetected.
    /// </summary>
    /// <returns>Minimum delay for Part 2.</returns>
    let solvePartTwo() = findDelay parseInput

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"