namespace _2017

open System

/// <summary>
/// Day 02: Corruption Checksum
///
/// Computes spreadsheet checksums using row extrema differences and evenly divisible value pairs.
/// </summary>
module _02 =
    let Data = (Utils.GetInputData 2).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Parses a tab-delimited string representing a spreadsheet row into an array of integers.
    /// </summary>
    /// <param name="line">Tab-separated string of numbers.</param>
    /// <returns>Array of integers in the row.</returns>
    let parseLine(line: string): int[] =
        line.Split('\t', StringSplitOptions.RemoveEmptyEntries) |> Array.map int

    /// <summary>
    /// Finds the unique pair of numbers in a row where one evenly divides the other and returns the quotient.
    /// </summary>
    /// <param name="numbers">Array of row numbers.</param>
    /// <returns>The quotient of the evenly divisible pair.</returns>
    let findEvenDivision(numbers: int[]) =
        numbers
        |> Array.collect(fun x -> numbers |> Array.map(fun y -> if x <> y && x % y = 0 then Some (x / y) else None))
        |> Array.choose id
        |> Array.head

    /// <summary>
    /// Solves Part 1: computes the spreadsheet checksum by summing the differences between max and min in each row.
    /// </summary>
    /// <returns>The total checksum for Part 1.</returns>
    let solvePartOne() =
        Data |> Array.fold(fun acc line ->
            let numbers = parseLine line
            acc + (Array.max numbers - Array.min numbers)
        ) 0

    /// <summary>
    /// Solves Part 2: computes the sum of quotients of evenly divisible numbers for each row.
    /// </summary>
    /// <returns>The total sum of quotients for Part 2.</returns>
    let solvePartTwo() =
        Data |> Array.fold(fun acc line ->
            let numbers = parseLine line
            acc + (findEvenDivision numbers)
        ) 0

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"