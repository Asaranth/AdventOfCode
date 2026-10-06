namespace _2017

open System
open System.Collections.Generic

/// <summary>
/// Day 07: Recursive Circus
///
/// Models tower hierarchy trees to find the root program and calculate corrected node weights for balance.
/// </summary>
module _07 =
    let Data = (Utils.GetInputData 7).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Parses a single line representing a program, its weight, and its held child programs.
    /// </summary>
    /// <param name="line">Raw text line containing program details.</param>
    /// <returns>A tuple of program name, its weight, and array of child names.</returns>
    let parseLine(line: string) =
        let nameWeightSplit = line.IndexOf('(')
        let program = line[..nameWeightSplit - 2].Trim()
        let weightSplit = line.IndexOf(')')
        let weight = line[nameWeightSplit + 1 .. weightSplit - 1].Trim() |> int
        let children =
            if line.Contains("->") then
                line.[weightSplit + 4 ..].Split(',', StringSplitOptions.TrimEntries)
            else
                [||]
        program, weight, children

    /// <summary>
    /// Solves Part 1: finds the root program at the bottom of the tower (not held by any other program).
    /// </summary>
    /// <returns>Name of the bottom program.</returns>
    let solvePartOne() =
        let programs = Dictionary<string, string[]>()
        let allPrograms = HashSet<string>()
        let supportedPrograms = HashSet<string>()
        Data
        |> Array.map parseLine
        |> Array.iter (fun (program, _, subs) ->
            programs[program] <- subs
            allPrograms.Add(program) |> ignore
            for sub in subs do supportedPrograms.Add(sub) |> ignore)
        allPrograms
        |> Seq.filter(fun p -> not (supportedPrograms.Contains(p)))
        |> Seq.exactlyOne

    /// <summary>
    /// Solves Part 2: finds the program with the incorrect weight and calculates what its weight should be to balance the tower.
    /// </summary>
    /// <returns>The corrected weight of the imbalanced program.</returns>
    let solvePartTwo() =
        let programs = Dictionary<string, string[]>()
        let weights = Dictionary<string, int>()
        let allPrograms = HashSet<string>()
        let supportedPrograms = HashSet<string>()
        Data
        |> Array.map parseLine
        |> Array.iter(fun (program, weight, subs) ->
            programs[program] <- subs
            weights[program] <- weight
            allPrograms.Add(program) |> ignore
            for sub in subs do supportedPrograms.Add(sub) |> ignore)
        let root = allPrograms
                   |> Seq.filter(fun p -> not (supportedPrograms.Contains(p)))
                   |> Seq.exactlyOne
        let rec calculateWeight(program: string): int =
            let children = programs[program]
            if children.Length = 0 then
                weights[program]
            else
                let childWeights = children |> Array.map calculateWeight
                let totalWeight = weights[program] + Array.sum childWeights
                totalWeight
        let isBalanced(weights: int[]) =
            let uniqueWeights = weights |> Array.distinct
            uniqueWeights.Length = 1
        let rec findImbalance (program: string) (expectedWeight: int) =
            let children = programs[program]
            if children.Length = 0 then
                program, expectedWeight
            else
                let childWeights = children |> Array.map(fun child -> child, calculateWeight child)
                let weightsOnly = childWeights |> Array.map snd
                if isBalanced weightsOnly then
                    program, expectedWeight
                else
                    let weightCount = weightsOnly |> Array.countBy id
                    let correctWeight, _ = weightCount |> Array.find(fun (_, count) -> count > 1)
                    let imbalanceWeight, _ = weightCount |> Array.find(fun (_, count) -> count = 1)
                    let imbalancedProgram, _ = childWeights |> Array.find(fun (_, weight) -> weight = imbalanceWeight)
                    let weightDifference = correctWeight - imbalanceWeight
                    findImbalance imbalancedProgram (weights[imbalancedProgram] + weightDifference)
        let _, correctedWeight = findImbalance root 0
        correctedWeight

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"