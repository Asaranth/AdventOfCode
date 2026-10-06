namespace _2017

open System
open System.Collections.Generic

/// <summary>
/// Day 16: Permutation Promenade
///
/// Simulates dance moves (spin, exchange, partner) on a 16-character sequence, using cycle detection for large iterations.
/// </summary>
module _16 =

    let Data = (Utils.GetInputData 16).Split(',', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

    /// <summary>
    /// Rotates the sequence by moving <paramref name="size"/> characters from the end to the front.
    /// </summary>
    /// <param name="size">Number of programs to spin from the end.</param>
    /// <param name="programs">The current list of program characters.</param>
    /// <returns>The rotated list of programs.</returns>
    let spin size programs =
        let splitIndex = List.length programs - size
        List.skip splitIndex programs @ List.take splitIndex programs

    /// <summary>
    /// Swaps the programs at indices <paramref name="posA"/> and <paramref name="posB"/>.
    /// </summary>
    /// <param name="posA">First index.</param>
    /// <param name="posB">Second index.</param>
    /// <param name="programs">The list of programs.</param>
    /// <returns>The list with swapped elements.</returns>
    let exchange posA posB programs =
        let swap lst i j =
            lst |> List.mapi (fun idx el ->
                if idx = i then lst[j]
                elif idx = j then lst[i]
                else el)
        swap programs posA posB

    /// <summary>
    /// Swaps the positions of named program characters <paramref name="charA"/> and <paramref name="charB"/>.
    /// </summary>
    /// <param name="charA">First character identifier.</param>
    /// <param name="charB">Second character identifier.</param>
    /// <param name="programs">The list of programs.</param>
    /// <returns>The list after partnering swap.</returns>
    let partner charA charB programs =
        let posA = List.findIndex(fun c -> c = charA) programs
        let posB = List.findIndex(fun c -> c = charB) programs
        exchange posA posB programs

    /// <summary>
    /// Parses and applies a single dance move instruction to the program list.
    /// </summary>
    /// <param name="programs">The list of programs.</param>
    /// <param name="move">Move string ('sX', 'xA/B', 'pA/B').</param>
    /// <returns>The updated list of programs.</returns>
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

    /// <summary>
    /// Executes one full dance routine across all moves in sequence.
    /// </summary>
    /// <param name="currentState">Initial program state.</param>
    /// <param name="moves">List of dance move instructions.</param>
    /// <returns>State after completing all moves.</returns>
    let dance currentState moves = moves |> List.fold performMove currentState

    /// <summary>
    /// Executes dance iterations using memoisation and cycle detection to skip repetitive periods.
    /// </summary>
    /// <param name="currentPrograms">Current list of program characters.</param>
    /// <param name="input">List of dance moves.</param>
    /// <param name="states">Dictionary mapping iteration count to program state.</param>
    /// <param name="i">Current iteration index.</param>
    /// <param name="times">Target total iteration count.</param>
    /// <returns>Final program list after all iterations.</returns>
    let rec findCycle currentPrograms input (states: Dictionary<int, char list>) i times =
        if i >= times then currentPrograms
        elif states.ContainsValue(currentPrograms) then states[times % i]
        else
            states[i] <- currentPrograms
            let newPrograms = dance currentPrograms input
            findCycle newPrograms input states (i + 1) times

    /// <summary>
    /// Simulates the dance for <paramref name="times"/> full iterations starting from 'a'..'p'.
    /// </summary>
    /// <param name="input">List of dance move instructions.</param>
    /// <param name="times">Total number of dance iterations.</param>
    /// <returns>String representing final order of programs.</returns>
    let solve (input: string list) (times: int) : string =
        let states = Dictionary<int, char list>()
        let finalState = findCycle ['a'..'p'] input states 0 times
        finalState |> List.map string |> String.concat ""

    /// <summary>
    /// Solves Part 1: finds the order of the programs after one full dance.
    /// </summary>
    /// <returns>Order string for Part 1.</returns>
    let solvePartOne() = solve Data 1

    /// <summary>
    /// Solves Part 2: finds the order of the programs after one billion dance iterations.
    /// </summary>
    /// <returns>Order string for Part 2.</returns>
    let solvePartTwo() = solve Data 1000000000

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"