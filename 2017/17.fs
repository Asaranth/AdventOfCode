namespace _2017

open System.Collections.Generic

module _17 =
    let Data = (Utils.GetInputData 17).Trim() |> int

    let getNextPosition currentPosition steps bufferSize = (currentPosition + steps) % bufferSize

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

    let solvePartTwo() =
        let steps = Data
        let mutable currentPosition = 0
        let mutable valueAfterZero = 0
        for i in 1..50000000 do
            currentPosition <- getNextPosition currentPosition steps i
            if currentPosition = 0 then valueAfterZero <- i
            currentPosition <- currentPosition + 1
        valueAfterZero

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"