namespace _2017

open System

module _02 =
    let Data = (Utils.GetInputData 2).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    let parseLine(line: string): int[] =
        line.Split('\t', StringSplitOptions.RemoveEmptyEntries) |> Array.map int

    let findEvenDivision(numbers: int[]) =
        numbers
        |> Array.collect(fun x -> numbers |> Array.map(fun y -> if x <> y && x % y = 0 then Some (x / y) else None))
        |> Array.choose id
        |> Array.head

    let solvePartOne() =
        Data |> Array.fold(fun sum line ->
            let numbers = parseLine line |> Array.sort
            sum + (numbers[numbers.Length - 1] - numbers[0])
        ) 0

    let solvePartTwo() =
        Data |> Array.fold(fun acc line ->
            let numbers = parseLine line
            acc + (findEvenDivision numbers)
        ) 0

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"