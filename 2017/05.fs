namespace _2017

open System;

/// <summary>
/// Day 05: A Maze of Twisty Trampolines, All Alike
///
/// Simulates instruction jump offsets with unconditional and conditional modification rules until escaping.
/// </summary>
module _05 =
    let Data = (Utils.GetInputData 5).Split('\n', StringSplitOptions.RemoveEmptyEntries) |> Array.map int

    /// <summary>
    /// Simulates execution of jump offsets until the pointer leaves the instruction array bounds.
    /// </summary>
    /// <param name="modifyJump">Function specifying how the current offset is modified after being visited.</param>
    /// <returns>Number of steps taken to exit the maze.</returns>
    let executeWithRule (modifyJump: int -> int) =
        let mutable instructions = Array.copy Data
        let mutable index = 0
        let mutable steps = 0
        while index >= 0 && index < instructions.Length do
            let jump = instructions.[index]
            instructions.[index] <- modifyJump jump
            index <- index + jump
            steps <- steps + 1
        steps

    /// <summary>
    /// Solves Part 1: counts steps to exit when every visited jump offset is incremented by 1.
    /// </summary>
    /// <returns>Number of steps for Part 1.</returns>
    let solvePartOne() = executeWithRule (fun jump -> jump + 1)

    /// <summary>
    /// Solves Part 2: counts steps to exit when offsets of 3 or more are decremented by 1, and others incremented by 1.
    /// </summary>
    /// <returns>Number of steps for Part 2.</returns>
    let solvePartTwo() = executeWithRule (fun jump -> if jump >= 3 then jump - 1 else jump + 1)

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"