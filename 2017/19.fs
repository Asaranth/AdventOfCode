namespace _2017

open System

/// <summary>
/// Day 19: A Series of Tubes
///
/// Traces ASCII grid routing network paths to collect letters and count total traversal steps.
/// </summary>
module _19 =
    let Data = (Utils.GetInputData 19).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Finds the starting horizontal index in the first row of the diagram.
    /// </summary>
    /// <param name="diagram">Array of ASCII network diagram lines.</param>
    /// <returns>X-coordinate of the vertical entry pipe '|'.</returns>
    let findStartPosition(diagram: string[]) = diagram[0].IndexOf('|')

    /// <summary>
    /// Checks whether a character is an alphabetical routing letter.
    /// </summary>
    /// <param name="c">Character to check.</param>
    /// <returns>True if alphabetical; false otherwise.</returns>
    let isLetter(c: char) = Char.IsLetter(c)

    /// <summary>
    /// Traces the full ASCII path from entrance until hitting an empty space or boundary.
    /// </summary>
    /// <param name="diagram">Array of ASCII diagram lines.</param>
    /// <param name="onVisit">Callback function invoked at every step with (x, y, char).</param>
    let traversePath(diagram: string[]) (onVisit: int -> int -> char -> unit) =
        let rows = diagram.Length
        let cols = diagram[0].Length
        let mutable x = findStartPosition diagram
        let mutable y = 0
        let mutable direction = (0, 1)
        let mutable continueWalking = true
        onVisit x y diagram[y].[x]

        while continueWalking do
            let dx, dy = direction
            x <- x + dx
            y <- y + dy
            if x < 0 || y < 0 || x >= cols || y >= rows || diagram[y].[x] = ' ' then continueWalking <- false
            else
                let charAtPos = diagram[y][x]
                onVisit x y charAtPos
                if charAtPos = '+' then
                    direction <-
                        if dx <> 0 then
                            if y > 0 && diagram[y - 1][x] <> ' ' then (0, -1)
                            else (0, 1)
                        else
                            if x > 0 && diagram[y][x - 1] <> ' ' then (-1, 0)
                            else (1, 0)

    /// <summary>
    /// Solves Part 1: collects all letters encountered along the network path in order.
    /// </summary>
    /// <returns>Concatenated letter sequence for Part 1.</returns>
    let solvePartOne() =
        let letters = System.Collections.Generic.List<char>()
        let onVisit _ _ charAtPos = if isLetter charAtPos then letters.Add charAtPos
        traversePath Data onVisit
        new string (letters.ToArray())

    /// <summary>
    /// Solves Part 2: counts the total number of steps walked along the path.
    /// </summary>
    /// <returns>Total step count for Part 2.</returns>
    let solvePartTwo() =
        let mutable steps = 0
        let onVisit _ _ _ = steps <- steps + 1
        traversePath Data onVisit
        steps

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"