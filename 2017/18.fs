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

    type State =
        | WaitingFor of string * Dictionary<string, int64> * int
        | Sent of int64 * Dictionary<string, int64> * int
        | Running of Dictionary<string, int64> * int
        | Terminated

    let parseInstruction(line: string) =
        let parts = line.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        match parts[0] with
        | "snd" -> Snd parts[1]
        | "set" -> Set (parts[1], parts[2])
        | "add" -> Add (parts[1], parts[2])
        | "mul" -> Mul (parts[1], parts[2])
        | "mod" -> Mod (parts[1], parts[2])
        | "rcv" -> Rcv parts[1]
        | "jgz" -> Jgz (parts[1], parts[2])
        | _ -> failwith "Unknown instruction"

    let getValue(registers: Dictionary<string, int64>) (operand: string) =
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

    let solvePartTwo() =
        let instructions = Data |> Array.map parseInstruction
        let runFor index registers =
            if index < 0 || index >= instructions.Length then Terminated
            else
                match instructions[index] with
                | Snd x -> Sent (getValue registers x, registers, index + 1)
                | Rcv x -> WaitingFor (x, registers, index + 1)
                | Set (x, y) ->
                    registers[x] <- getValue registers y
                    Running (registers, index + 1)
                | Add (x, y) ->
                    registers[x] <- (getValue registers x) + (getValue registers y)
                    Running (registers, index + 1)
                | Mul (x, y) ->
                    registers[x] <- (getValue registers x) * (getValue registers y)
                    Running (registers, index + 1)
                | Mod (x, y) ->
                    registers[x] <- (getValue registers x) % (getValue registers y)
                    Running (registers, index + 1)
                | Jgz (x, y) ->
                    if getValue registers x > 0L then Running (registers, index + int (getValue registers y))
                    else Running (registers, index + 1)

        let mutable p1State = Running (Dictionary(dict [("p", 0L)]), 0)
        let mutable p2State = Running (Dictionary(dict [("p", 1L)]), 0)
        let mutable p1Queue, p2Queue = List.empty, List.empty
        let mutable running, sentCount = true, 0

        while running do
            match p1State, p2State with
            | Terminated, Terminated -> running <- false
            | WaitingFor _, WaitingFor _ when p1Queue.IsEmpty && p2Queue.IsEmpty -> running <- false
            | Terminated, WaitingFor _ when p1Queue.IsEmpty -> running <- false
            | WaitingFor _, Terminated when p2Queue.IsEmpty -> running <- false
            | _ ->
                match p1State with
                | WaitingFor (reg, regs, index) when not p2Queue.IsEmpty ->
                    regs[reg] <- List.head p2Queue
                    p1State <- runFor index regs
                    p2Queue <- List.tail p2Queue
                | Sent (value, regs, index) ->
                    p1Queue <- p1Queue @ [value]
                    p1State <- runFor index regs
                | Running (regs, index) -> p1State <- runFor index regs
                | _ -> ()

                match p2State with
                | WaitingFor (reg, regs, index) when not p1Queue.IsEmpty ->
                    regs[reg] <- List.head p1Queue
                    p2State <- runFor index regs
                    p1Queue <- List.tail p1Queue
                | Sent (value, regs, index) ->
                    p2Queue <- p2Queue @ [value]
                    sentCount <- sentCount + 1
                    p2State <- runFor index regs
                | Running (regs, index) -> p2State <- runFor index regs
                | _ -> ()

        sentCount

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"