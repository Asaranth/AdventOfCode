namespace _2017

open System

/// <summary>
/// Day 11: Hex Ed
///
/// Navigates a hexagonal coordinate grid using axial coordinates and computes hex Manhattan distances.
/// </summary>
module _11 =
    let Data = (Utils.GetInputData 11).Split(',', StringSplitOptions.RemoveEmptyEntries) |> Array.map(_.Trim())

    let directions =
        dict [
            "n",  (0, -1)
            "ne", (1, -1)
            "se", (1, 0)
            "s",  (0, 1)
            "sw", (-1, 1)
            "nw", (-1, 0)
        ]

    /// <summary>
    /// Adds two axial coordinate pairs.
    /// </summary>
    /// <param name="q1">First coordinate (q, r).</param>
    /// <param name="q2">Second coordinate (q, r).</param>
    /// <returns>Summed axial coordinates.</returns>
    let add (q1, r1) (q2, r2) = (q1 + q2, r1 + r2)

    /// <summary>
    /// Calculates the hex grid distance between two points in axial coordinates.
    /// </summary>
    /// <param name="q1">First point (q, r).</param>
    /// <param name="q2">Second point (q, r).</param>
    /// <returns>Step distance across the hexagonal grid.</returns>
    let distance (q1, r1) (q2, r2) = (abs (q1 - q2) + abs ((q1 + r1) - (q2 + r2)) + abs (r1 - r2)) / 2

    /// <summary>
    /// Traverses all directional steps, calculating either the final distance or maximum distance reached from origin.
    /// </summary>
    /// <param name="isPartTwo">If true, returns the maximum distance reached; otherwise, returns the final distance.</param>
    /// <returns>Hex grid step distance.</returns>
    let solve(isPartTwo: bool) =
        let finalPosition, maxDistance =
            Data |> Array.fold (fun (acc, maxDist) dir ->
                let newPos = add acc directions[dir]
                newPos, max maxDist (distance (0, 0) newPos)
            ) ((0, 0), 0)
        if isPartTwo then maxDistance else distance (0, 0) finalPosition


    /// <summary>
    /// Solves Part 1: calculates the shortest distance to the child process at the end of the walk.
    /// </summary>
    /// <returns>Hex grid distance for Part 1.</returns>
    let solvePartOne() = solve false

    /// <summary>
    /// Solves Part 2: calculates the maximum distance the child process ever reached from the starting position.
    /// </summary>
    /// <returns>Maximum hex grid distance reached for Part 2.</returns>
    let solvePartTwo() = solve true

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"