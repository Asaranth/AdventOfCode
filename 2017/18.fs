namespace _2017

open System
open System.Collections.Generic

module _18 =
    let Data = (Utils.GetInputData 18).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    type Instruction =
        | Snd of string
        | Set of string * string
        | Add of string * string
        | Mul of string * string
        | Mod of string * string
        | Rcv of string
        | Jgz of string * string

    let parseInstruction (line: string) =
        let parts = line.Split(' ')
        match parts[0] with
        | "snd" -> Snd parts[1]
        | "set" -> Set (parts[1], parts[2])
        | "add" -> Add (parts[1], parts[2])
        | "mul" -> Mul (parts[1], parts[2])
        | "mod" -> Mod (parts[1], parts[2])
        | "rcv" -> Rcv parts[1]
        | "jgz" -> Jgz (parts[1], parts[2])
        | _ -> failwith "Unknown instruction"

    let getValue (registers: Dictionary<string, int64>) (operand: string) =
        match Int64.TryParse(operand) with
        | true, value -> value
        | false, _ -> if registers.ContainsKey(operand) then registers[operand] else 0L

    let solvePartOne() =
        let instructions = Data |> Array.map parseInstruction
        let registers = Dictionary<string, int64>()
        let mutable sound = 0L
        let mutable pc = 0
        let mutable recoveredFrequency = None

        while pc >= 0 && pc < instructions.Length && recoveredFrequency.IsNone do
            match instructions[pc] with
            | Snd x ->
                sound <- getValue registers x
                pc <- pc + 1
            | Set (x, y) ->
                registers[x] <- getValue registers y
                pc <- pc + 1
            | Add (x, y) ->
                registers[x] <- (getValue registers x) + (getValue registers y)
                pc <- pc + 1
            | Mul (x, y) ->
                registers[x] <- (getValue registers x) * (getValue registers y)
                pc <- pc + 1
            | Mod (x, y) ->
                registers[x] <- (getValue registers x) % (getValue registers y)
                pc <- pc + 1
            | Rcv x ->
                if getValue registers x <> 0L then
                    recoveredFrequency <- Some sound
                else
                    pc <- pc + 1
            | Jgz (x, y) ->
                if getValue registers x > 0L then
                    pc <- pc + int (getValue registers y)
                else
                    pc <- pc + 1

        match recoveredFrequency with
        | Some freq -> int freq
        | None -> -1

    let solvePartTwo() = -1

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"