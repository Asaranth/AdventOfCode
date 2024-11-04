namespace _2017

open System

module _15 =
    let Data = (Utils.GetInputData 15).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    let parseData(prefix: string) = Data |> Array.find (_.StartsWith(prefix)) |> fun line -> line.Split(' ').[4] |> int64

    let generateNextValue (previousValue, factor) = (previousValue * factor) % 2147483647L

    let lowest16Bits value = value &&& 0xFFFFL

    let countMatchingPairs iterations filterA filterB =
        let mutable count = 0
        let mutable valueA = parseData "Generator A starts with"
        let mutable valueB = parseData "Generator B starts with"
        let mutable validPairs = 0
        while validPairs < iterations do
            valueA <- generateNextValue (valueA, 16807L)
            if filterA valueA then
                valueB <- generateNextValue (valueB, 48271L)
                while not (filterB valueB) do valueB <- generateNextValue (valueB, 48271L)
                if (lowest16Bits valueA) = (lowest16Bits valueB) then count <- count + 1
                validPairs <- validPairs + 1
        count

    let solvePartOne() = countMatchingPairs 40000000 (fun _ -> true) (fun _ -> true)

    let solvePartTwo() = countMatchingPairs 5000000 (fun value -> value % 4L = 0L) (fun value -> value % 8L = 0L)

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"