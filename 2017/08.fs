namespace _2017

open System
open System.Collections.Generic

module _08 =
    let Data = (Utils.GetInputData 8).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    let parseInstruction(instruction: string) =
        let parts = instruction.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        (parts[0], parts[1], int parts[2], parts[4], parts[5], int parts[6])

    let evaluateCondition(registers: Dictionary<string, int>) (condReg: string, condOp: string, condVal: int) =
        let regValue = if registers.ContainsKey(condReg) then registers[condReg] else 0
        match condOp with
        | ">" -> regValue > condVal
        | "<" -> regValue < condVal
        | ">=" -> regValue >= condVal
        | "<=" -> regValue <= condVal
        | "==" -> regValue = condVal
        | "!=" -> regValue <> condVal
        | _ -> false

    let processInstruction(registers: Dictionary<string, int>) (reg: string, op: string, value: int) =
        if not (registers.ContainsKey(reg)) then registers[reg] <- 0
        match op with
        | "inc" -> registers[reg] <- registers[reg] + value
        | "dec" -> registers[reg] <- registers[reg] - value
        | _ -> ()

    let processInstructions(instructions: string[]) =
        let registers = Dictionary<string, int>()
        let mutable highestValueEver = Int32.MinValue
        for instruction in instructions do
            let reg, op, value, condReg, condOp, condVal = parseInstruction instruction
            if evaluateCondition registers (condReg, condOp, condVal) then
                processInstruction registers (reg, op, value)
                highestValueEver <- max highestValueEver (if registers.ContainsKey(reg) then registers[reg] else 0)
        registers, highestValueEver

    let findMaxValue(registers: Dictionary<string, int>) =
        if registers.Count = 0 then 0
        else registers.Values |> Seq.max

    let solvePartOne() =
        let registers, _ = processInstructions Data
        findMaxValue registers

    let solvePartTwo() =
        let _, highestValueEver = processInstructions Data
        highestValueEver

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"