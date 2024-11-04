namespace _2017

open System

module _13 =
    let Data = (Utils.GetInputData 13).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    let parseInput = Data |> Array.map(fun line ->
        let parts = line.Split(':', StringSplitOptions.RemoveEmptyEntries)
        Int32.Parse(parts[0]), Int32.Parse(parts[1].Trim()))

    let calculateSeverity firewall =
        firewall |> Array.fold(fun severity (depth, range) ->
            let cycle = (range - 1) * 2
            if depth % cycle = 0 then severity + (depth * range)
            else severity) 0

    let isCaught delay firewall =
        firewall |> Array.exists(fun (depth, range) ->
            let cycle = (range - 1) * 2
            (depth + delay) % cycle = 0)

    let findDelay firewall =
        let rec search delay =
            if not (isCaught delay firewall) then delay
            else search (delay + 1)
        search 0

    let solvePartOne() = calculateSeverity parseInput

    let solvePartTwo() = findDelay parseInput

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"