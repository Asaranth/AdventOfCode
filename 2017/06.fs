namespace _2017

open System
open System.Collections.Generic;

module _06 =
    let Data = (Utils.GetInputData 6).Split('\t', StringSplitOptions.RemoveEmptyEntries) |> Array.map int

    let redistribute(banks: int[]) =
        let len = banks.Length
        let maxBlocks = Array.max banks
        let index = Array.findIndex(fun i -> i = maxBlocks) banks
        banks[index] <- 0
        for i in 1 .. maxBlocks do
            banks[(index + i) % len] <- banks[(index + i) % len] + 1

    let solvePartOne() =
        let seenConfigurations = HashSet<string>()
        let rec distribute cycles =
            let config = String.Join(',', Data)
            if seenConfigurations.Contains(config) then cycles
            else
                seenConfigurations.Add(config) |> ignore
                redistribute Data
                distribute (cycles + 1)
        distribute 0

    let solvePartTwo() =
        let seenConfigurations = Dictionary<string, int>()
        let rec distribute (banks: int[]) cycles =
            let config = String.Join(',', banks)
            if seenConfigurations.ContainsKey(config) then
                cycles - seenConfigurations.[config]
            else
                seenConfigurations[config] <- cycles
                redistribute banks
                distribute banks (cycles + 1)
        distribute (Array.copy Data) 0

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"