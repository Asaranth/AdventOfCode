namespace _2017

open System
open System.Collections.Generic

module _07 =
    let Data = (Utils.GetInputData 7).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    let parseLine(line: string) =
        if line.Contains("->") then
            let parts = line.Split("->", StringSplitOptions.TrimEntries)
            let program = parts[0].Split(' ')[0]
            let subPrograms = parts[1].Split(',', StringSplitOptions.TrimEntries)
            program, subPrograms
        else
            let program = line.Split(' ')[0]
            program, [||]

    let solvePartOne() =
        let programs = Dictionary<string, string[]>()
        let allPrograms = HashSet<string>()
        let supportedPrograms = HashSet<string>()
        Data |> Array.map parseLine |> Array.iter(fun (program, subs) ->
            programs[program] <- subs
            allPrograms.Add(program) |> ignore
            for sub in subs do supportedPrograms.Add(sub) |> ignore)
        allPrograms |> Seq.filter(fun p -> not (supportedPrograms.Contains(p))) |> Seq.exactlyOne

    let solvePartTwo() = -1

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"