namespace _2017

open System
open System.Collections.Generic

module _16 =

    let Data = (Utils.GetInputData 16).Split(',', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

    let spin size programs =
        let splitIndex = List.length programs - size
        List.skip splitIndex programs @ List.take splitIndex programs

    let exchange posA posB programs =
        let swap lst i j =
            lst |> List.mapi (fun idx el ->
                if idx = i then lst[j]
                elif idx = j then lst[i]
                else el)
        swap programs posA posB

    let partner charA charB programs =
        let posA = List.findIndex(fun c -> c = charA) programs
        let posB = List.findIndex(fun c -> c = charB) programs
        exchange posA posB programs

    let performMove programs (move: string) =
        match move[0] with
        | 's' ->
            let size = move[1..] |> int
            spin size programs
        | 'x' ->
            let positions = move[1..].Split('/')
            let posA = positions[0] |> int
            let posB = positions[1] |> int
            exchange posA posB programs
        | 'p' ->
            let chars = move[1..].Split('/')
            let charA = chars[0].[0]
            let charB = chars[1].[0]
            partner charA charB programs
        | _ -> programs

    let dance currentState moves = moves |> List.fold performMove currentState

    let rec findCycle currentPrograms input (states: Dictionary<int, char list>) i times =
        if i >= times then currentPrograms
        elif states.ContainsValue(currentPrograms) then states[times % i]
        else
            states[i] <- currentPrograms
            let newPrograms = dance currentPrograms input
            findCycle newPrograms input states (i + 1) times

    let solve (input: string list) (times: int) : string =
        let states = Dictionary<int, char list>()
        let finalState = findCycle ['a'..'p'] input states 0 times
        finalState |> List.map string |> String.concat ""

    let solvePartOne() = solve Data 1

    let solvePartTwo() = solve Data 1000000000

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"