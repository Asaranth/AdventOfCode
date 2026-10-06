namespace _2017

/// <summary>
/// Day 01: Inverse Captcha
///
/// Solves digit-sequence captchas by summing digits matching neighbours with circular offsets.
/// </summary>
module _01 =
    let Data = (Utils.GetInputData 1).Trim()

    /// <summary>
    /// Parses the puzzle input string into an array of integer digits.
    /// </summary>
    /// <returns>Array of individual digits.</returns>
    let getDigits() = Data |> Seq.map(fun c -> int c - int '0') |> Seq.toArray

    /// <summary>
    /// Computes the sum of digits that match the circular element at a specified offset.
    /// </summary>
    /// <param name="digits">The array of digits.</param>
    /// <param name="len">The length of the digits array.</param>
    /// <param name="offset">The circular forward offset to compare against.</param>
    /// <returns>Sum of matching digits.</returns>
    let calculateSum digits len offset =
        digits |> Array.mapi(fun i digit -> if digit = digits[(i + offset) % len] then digit else 0) |> Array.sum

    /// <summary>
    /// Solves Part 1: calculates the sum of digits matching their immediate circular neighbour.
    /// </summary>
    /// <returns>The resulting sum for Part 1.</returns>
    let solvePartOne() =
        let digits = getDigits()
        calculateSum digits digits.Length 1

    /// <summary>
    /// Solves Part 2: calculates the sum of digits matching the element halfway around the list.
    /// </summary>
    /// <returns>The resulting sum for Part 2.</returns>
    let solvePartTwo() =
        let digits = getDigits()
        calculateSum digits digits.Length (digits.Length / 2)

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"