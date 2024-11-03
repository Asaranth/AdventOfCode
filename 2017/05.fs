namespace _2017

open System;

module _05 =
    let Data = (Utils.GetInputData 5).Split('\n', StringSplitOptions.RemoveEmptyEntries) |> Array.map int

    let executeWithRule (modifyJump: int -> int) =
        let mutable instructions = Array.copy Data
        let mutable index = 0
        let mutable steps = 0
        while index >= 0 && index < instructions.Length do
            let jump = instructions.[index]
            instructions.[index] <- modifyJump jump
            index <- index + jump
            steps <- steps + 1
        steps

    let solvePartOne() = executeWithRule (fun jump -> jump + 1)

    let solvePartTwo() = executeWithRule (fun jump -> if jump >= 3 then jump - 1 else jump + 1)


    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"