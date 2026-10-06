namespace _2017

open System
open System.Collections.Generic

/// <summary>
/// Day 22: Sporifica Virus
///
/// Simulates virus carrier movements and infection state transitions across an infinite 2D grid.
/// </summary>
module _22 =
    let Data = (Utils.GetInputData 22).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Cardinal movement direction of the virus carrier.
    /// </summary>
    type Direction = Up | Right | Down | Left

    /// <summary>
    /// Health state of a grid node.
    /// </summary>
    type NodeState = Clean | Weakened | Infected | Flagged

    /// <summary>
    /// Turns direction 90 degrees left (counter-clockwise).
    /// </summary>
    /// <param name="direction">Current heading.</param>
    /// <returns>New heading.</returns>
    let turnLeft direction =
        match direction with
        | Up -> Left
        | Right -> Up
        | Down -> Right
        | Left -> Down

    /// <summary>
    /// Turns direction 90 degrees right (clockwise).
    /// </summary>
    /// <param name="direction">Current heading.</param>
    /// <returns>New heading.</returns>
    let turnRight direction =
        match direction with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    /// <summary>
    /// Reverses the current movement direction by 180 degrees.
    /// </summary>
    /// <param name="direction">Current heading.</param>
    /// <returns>Opposite heading.</returns>
    let reverse direction =
        match direction with
        | Up -> Down
        | Right -> Left
        | Down -> Up
        | Left -> Right

    /// <summary>
    /// Advances coordinate (x, y) by one step along the given direction.
    /// </summary>
    /// <param name="x">Current x position.</param>
    /// <param name="y">Current y position.</param>
    /// <param name="direction">Movement heading.</param>
    /// <returns>New coordinate pair.</returns>
    let move (x, y) direction =
        match direction with
        | Up -> (x, y - 1)
        | Right -> (x + 1, y)
        | Down -> (x, y + 1)
        | Left -> (x - 1, y)

    /// <summary>
    /// Solves Part 1: simulates 10,000 virus carrier bursts with simple Clean/Infected binary state transitions.
    /// </summary>
    /// <returns>Count of bursts that cause a node to become infected.</returns>
    let solvePartOne() =
        let grid = Dictionary<int * int, bool>()
        for y in 0 .. Data.Length - 1 do
            for x in 0 .. Data[y].Length - 1 do
                let infected = Data[y].[x] = '#'
                if infected then grid[(x, y)] <- true
        let middle = Data.Length / 2
        let mutable position = (middle, middle)
        let mutable direction = Up
        let mutable infections = 0
        for _ in 1 .. 10000 do
            let isInfected = grid.ContainsKey(position) && grid[position]
            direction <- if isInfected then turnRight direction else turnLeft direction
            if isInfected then grid[position] <- false
            else
                grid[position] <- true
                infections <- infections + 1
            position <- move position direction
        infections

    /// <summary>
    /// Solves Part 2: simulates 10,000,000 bursts with 4-state transitions (Clean -&gt; Weakened -&gt; Infected -&gt; Flagged -&gt; Clean).
    /// </summary>
    /// <returns>Count of bursts that cause a node to become infected.</returns>
    let solvePartTwo() =
        let grid = Dictionary<int * int, NodeState>()
        for y in 0 .. Data.Length - 1 do
            for x in 0 .. Data[y].Length - 1 do
                let state = if Data[y].[x] = '#' then Infected else Clean
                grid[(x, y)] <- state
        let middle = Data.Length / 2
        let mutable position = (middle, middle)
        let mutable direction = Up
        let mutable infections = 0
        for _ in 1 .. 10000000 do
            let state =
                if grid.ContainsKey(position) then grid[position]
                else Clean
            direction <-
                match state with
                | Clean -> turnLeft direction
                | Weakened -> direction
                | Infected -> turnRight direction
                | Flagged -> reverse direction
            match state with
            | Clean -> grid[position] <- Weakened
            | Weakened ->
                grid[position] <- Infected
                infections <- infections + 1
            | Infected -> grid[position] <- Flagged
            | Flagged -> grid[position] <- Clean
            position <- move position direction
        infections

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"