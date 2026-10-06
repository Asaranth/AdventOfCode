namespace _2017

open System.Collections.Generic

/// <summary>
/// Day 17: Spinlock
///
/// Simulates circular buffer insertions and tracks values after insertion points up to 50 million iterations.
/// </summary>
module _17 =
    let Data = (Utils.GetInputData 17).Trim() |> int

    /// <summary>
    /// Calculates the next insertion index in a circular buffer after stepping forward.
    /// </summary>
    /// <param name="currentPosition">Current index in the buffer.</param>
    /// <param name="steps">Number of step advances per insertion.</param>
    /// <param name="bufferSize">Current size of the buffer.</param>
    /// <returns>Index position before insertion.</returns>
    let getNextPosition currentPosition steps bufferSize = (currentPosition + steps) % bufferSize

    /// <summary>
    /// Solves Part 1: inserts 2017 values into the circular list and finds the value immediately following 2017.
    /// </summary>
    /// <returns>Value following 2017 in the buffer for Part 1.</returns>
    let solvePartOne() =
        let steps = Data
        let buffer = List<int>()
        buffer.Add(0)
        let mutable currentPosition = 0
        for i in 1..2017 do
            currentPosition <- getNextPosition currentPosition steps buffer.Count
            buffer.Insert(currentPosition + 1, i)
            currentPosition <- currentPosition + 1
        let positionOf2017 = buffer.IndexOf(2017)
        buffer[(positionOf2017 + 1) % buffer.Count]

    /// <summary>
    /// Solves Part 2: tracks the value at index 1 (directly after 0) over 50 million insertions without storing the full buffer.
    /// </summary>
    /// <returns>Value after 0 after 50 million insertions for Part 2.</returns>
    let solvePartTwo() =
        let steps = Data
        let mutable currentPosition = 0
        let mutable valueAfterZero = 0
        for i in 1..50000000 do
            currentPosition <- getNextPosition currentPosition steps i
            if currentPosition = 0 then valueAfterZero <- i
            currentPosition <- currentPosition + 1
        valueAfterZero

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"