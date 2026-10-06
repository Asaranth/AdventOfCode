namespace _2017

open System

/// <summary>
/// Day 09: Stream Processing
///
/// Parses nested group streams and filters garbage content containing cancelled characters.
/// </summary>
module _09 =
    let Data = (Utils.GetInputData 9).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Processes a character stream, maintaining garbage state and skipping cancelled characters.
    /// </summary>
    /// <param name="processChar">Callback invoked for each uncancelled character and its garbage status.</param>
    let processStream processChar =
        let stream = Data[0]
        let mutable inGarbage = false
        let mutable skipNext = false
        for c in stream do
            match (skipNext, inGarbage, c) with
            | true, _, _ -> skipNext <- false
            | _, _, '!' -> skipNext <- true
            | _, true, '>' -> inGarbage <- false
            | _, true, _ -> processChar (c, inGarbage)
            | _, _, '<' -> inGarbage <- true
            | _, _, _ -> processChar (c, inGarbage)

    /// <summary>
    /// Solves Part 1: computes the total score across all nested groups.
    /// </summary>
    /// <returns>Total score of all valid groups.</returns>
    let solvePartOne() =
        let mutable score = 0
        let mutable depth = 0
        processStream (fun (c, inGarbage) ->
            if not inGarbage then
                match c with
                | '{' ->
                    depth <- depth + 1
                    score <- score + depth
                | '}' -> depth <- depth - 1
                | _ -> ())
        score

    /// <summary>
    /// Solves Part 2: counts the total number of non-cancelled characters contained within garbage.
    /// </summary>
    /// <returns>Total count of garbage characters.</returns>
    let solvePartTwo() =
        let mutable garbageCount = 0
        processStream (fun (_, inGarbage) -> if inGarbage then garbageCount <- garbageCount + 1)
        garbageCount

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"