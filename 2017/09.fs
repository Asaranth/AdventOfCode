namespace _2017

open System

module _09 =
    let Data = (Utils.GetInputData 9).Split('\n', StringSplitOptions.RemoveEmptyEntries)

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

    let solvePartTwo() =
        let mutable garbageCount = 0
        processStream (fun (_, inGarbage) -> if inGarbage then garbageCount <- garbageCount + 1)
        garbageCount

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"