namespace _2017

open System
open System.Text.RegularExpressions
open System.Collections.Generic

module _25 =
    let Data: string[] = (Utils.GetInputData 25).Split([|"\n\n"|], StringSplitOptions.RemoveEmptyEntries)

    type Action = { Write: int; Move: int; NextState: char; }

    type State = { WhenZero: Action; WhenOne: Action; }

    let parseAction(lines: string[]) startIndex =
        let writeLine = lines[startIndex + 1].Trim()
        let write = Int32.Parse(writeLine.Substring(writeLine.Length - 2, 1))
        let moveLine = lines[startIndex + 2].Trim()
        let move = if moveLine.EndsWith("right.") then 1 else -1
        let nextStateLine = lines[startIndex + 3].Trim()
        let nextState = nextStateLine.Substring(nextStateLine.Length - 2, 1).[0]
        { Write = write; Move = move; NextState = nextState }

    let parseState(stateStr: string) =
        let lines = stateStr.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        let stateChar = lines[0].[lines[0].Length - 2]
        let whenZero = parseAction lines 1
        let whenOne = parseAction lines 5
        (stateChar, { WhenZero = whenZero; WhenOne = whenOne })

    let states = Data[1..] |> Array.map parseState |> dict |> fun d -> Dictionary<char, State>(d)

    let initialState =
        let initLine = Data[0].Split('\n', StringSplitOptions.RemoveEmptyEntries).[0]
        initLine[initLine.Length - 2]

    let steps =
        let stepsLine = Data[0].Split('\n', StringSplitOptions.RemoveEmptyEntries).[1]
        Int32.Parse(Regex.Match(stepsLine, @"\d+").Value)

    let simulate (states: Dictionary<char, State>) initialState steps =
        let tape = Dictionary<int, int>()
        let mutable currentPosition = 0
        let mutable currentState = initialState
        for _ in 1..steps do
            let currentValue = if tape.ContainsKey(currentPosition) then tape[currentPosition] else 0
            let action =
                match currentValue with
                | 0 -> states[currentState].WhenZero
                | 1 -> states[currentState].WhenOne
                | _ -> failwith "Unexpected tape value"
            tape[currentPosition] <- action.Write
            currentPosition <- currentPosition + action.Move
            currentState <- action.NextState
        tape.Values |> Seq.filter (fun v -> v = 1) |> Seq.length

    let solve() = simulate states initialState steps

    let Run() = printfn $"Solution: {solve()}"