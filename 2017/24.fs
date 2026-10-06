namespace _2017

open System

/// <summary>
/// Day 24: Electromagnetic Moat
///
/// Constructs valid port-matching domino component bridges to maximise overall strength and bridge length.
/// </summary>
module _24 =
    let Data = (Utils.GetInputData 24).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Represents a magnetic component with two connecting port numbers.
    /// </summary>
    type Component = int * int

    /// <summary>
    /// Parses input lines into a list of two-port <see cref="Component"/> tuples.
    /// </summary>
    let parseComponents = Data |> Array.map (fun line ->
        let ports = line.Split('/')
        int ports[0], int ports[1]) |> Array.toList

    /// <summary>
    /// Calculates total strength of a bridge by summing all component port values.
    /// </summary>
    /// <param name="bridge">List of components in the bridge.</param>
    /// <returns>Sum of port values.</returns>
    let bridgeStrength(bridge: Component list) = List.sumBy (fun (a, b) -> a + b) bridge

    /// <summary>
    /// Recursively searches for the bridge with the maximum total strength using backtracking.
    /// </summary>
    /// <param name="components">Available remaining components.</param>
    /// <param name="currentPort">Port type required for the next connection.</param>
    /// <param name="currentBridge">Accumulated bridge components so far.</param>
    /// <returns>The strongest valid component bridge.</returns>
    let rec findStrongest(components: Component list) currentPort (currentBridge: Component list) =
        let possibleComponents = components |> List.filter (fun (a, b) -> a = currentPort || b = currentPort)

        if List.isEmpty possibleComponents then currentBridge
        else
            let allBridges = possibleComponents |> List.collect (fun c ->
                let remainingComponents = components |> List.except [c]
                let nextPort = if fst c = currentPort then snd c else fst c
                [findStrongest remainingComponents nextPort (c :: currentBridge)])

            List.maxBy bridgeStrength (currentBridge :: allBridges)

    /// <summary>
    /// Recursively searches for the longest bridge, breaking ties by selecting the strongest.
    /// </summary>
    /// <param name="components">Available remaining components.</param>
    /// <param name="currentPort">Port type required for the next connection.</param>
    /// <param name="currentBridge">Accumulated bridge components so far.</param>
    /// <returns>A tuple of (best bridge list, bridge length, bridge strength).</returns>
    let rec findLongestAndStrongest (components: Component list) currentPort (currentBridge: Component list) =
        let possibleComponents = components |> List.filter (fun (a, b) -> a = currentPort || b = currentPort)
        if List.isEmpty possibleComponents then (currentBridge, List.length currentBridge, bridgeStrength currentBridge)
        else
            let allBridges = possibleComponents |> List.collect (fun c ->
                let remainingComponents = components |> List.except [c]
                let nextPort = if fst c = currentPort then snd c else fst c
                let bridge, length, strength = findLongestAndStrongest remainingComponents nextPort (c :: currentBridge)
                [(bridge, length, strength)])

            let longestBridge, longestLength, strongestStrength = allBridges |> List.maxBy (fun (_, length, strength) -> length, strength)
            if longestLength > List.length currentBridge then (longestBridge, longestLength, strongestStrength)
            else (currentBridge, List.length currentBridge, bridgeStrength currentBridge)

    /// <summary>
    /// Solves Part 1: finds the strength of the strongest bridge that can be built starting with port 0.
    /// </summary>
    /// <returns>Maximum strength for Part 1.</returns>
    let solvePartOne() =
        let strongestBridge = findStrongest parseComponents 0 []
        bridgeStrength strongestBridge

    /// <summary>
    /// Solves Part 2: finds the strength of the longest bridge (resolving ties with maximum strength).
    /// </summary>
    /// <returns>Strength of the longest bridge for Part 2.</returns>
    let solvePartTwo() =
        let longestAndStrongestBridge, _, _ = findLongestAndStrongest parseComponents 0 []
        bridgeStrength longestAndStrongestBridge

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"