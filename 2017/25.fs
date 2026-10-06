namespace _2017

open System
open System.Text.RegularExpressions
open System.Collections.Generic

/// <summary>
/// Day 25: The Halting Problem
///
/// Simulates a state machine Turing tape diagnostic checksum program for a specified number of steps.
/// </summary>
module _25 =
    let Data: string[] = (Utils.GetInputData 25).Split([|"\n\n"|], StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Turing machine action specifying the value to write, cursor movement offset, and next state identifier.
    /// </summary>
    type Action = { Write: int; Move: int; NextState: char; }

    /// <summary>
    /// Branching state transitions for 0 and 1 tape values.
    /// </summary>
    type State = { WhenZero: Action; WhenOne: Action; }

    /// <summary>
    /// Parses an action block (write, move direction, next state) from blueprint lines.
    /// </summary>
    /// <param name="lines">Array of blueprint text lines.</param>
    /// <param name="startIndex">Starting line index of the condition block.</param>
    /// <returns>Action configuration record.</returns>
    let parseAction(lines: string[]) startIndex =
        let writeLine = lines[startIndex + 1].Trim()
        let write = Int32.Parse(writeLine.Substring(writeLine.Length - 2, 1))
        let moveLine = lines[startIndex + 2].Trim()
        let move = if moveLine.EndsWith("right.") then 1 else -1
        let nextStateLine = lines[startIndex + 3].Trim()
        let nextState = nextStateLine.Substring(nextStateLine.Length - 2, 1).[0]
        { Write = write; Move = move; NextState = nextState }

    /// <summary>
    /// Parses a complete state transition block for a named state.
    /// </summary>
    /// <param name="stateStr">Raw multiline text of the state definition.</param>
    /// <returns>Tuple of state character and its <see cref="State"/> definition.</returns>
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

    /// <summary>
    /// Simulates the Turing machine on an infinite tape for the given step count.
    /// </summary>
    /// <param name="states">State transitions dictionary.</param>
    /// <param name="initialState">Starting state character.</param>
    /// <param name="steps">Total simulation steps to run.</param>
    /// <returns>Diagnostic checksum (count of 1s on the tape).</returns>
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

    /// <summary>
    /// Solves the halting problem checksum by simulating the machine up to the specified diagnostic step.
    /// </summary>
    /// <returns>Diagnostic checksum count.</returns>
    let solve() = simulate states initialState steps

    /// <summary>
    /// Executes and prints the solution.
    /// </summary>
    let Run() = printfn $"Solution: {solve()}"