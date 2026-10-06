namespace _2017

open System
open System.Collections.Generic

/// <summary>
/// Day 08: I Heard You Like Registers
///
/// Simulates register instructions with conditional execution, tracking current and historical maximum register values.
/// </summary>
module _08 =
    let Data = (Utils.GetInputData 8).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Parses a single instruction string into register operation and condition components.
    /// </summary>
    /// <param name="instruction">Instruction line string.</param>
    /// <returns>A tuple of (target register, operation, value, condition register, comparison operator, condition value).</returns>
    let parseInstruction(instruction: string) =
        let parts = instruction.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        (parts[0], parts[1], int parts[2], parts[4], parts[5], int parts[6])

    /// <summary>
    /// Evaluates whether an instruction condition holds true based on current register states.
    /// </summary>
    /// <param name="registers">Dictionary mapping register names to integer values.</param>
    /// <param name="condReg">Register to test in the condition.</param>
    /// <param name="condOp">Comparison operator ("&gt;", "&lt;", "&gt;=", "&lt;=", "==", "!=").</param>
    /// <param name="condVal">Integer value to compare against.</param>
    /// <returns>True if the condition is satisfied; false otherwise.</returns>
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

    /// <summary>
    /// Applies an increment or decrement operation to the target register.
    /// </summary>
    /// <param name="registers">Dictionary mapping register names to integer values.</param>
    /// <param name="reg">Target register name.</param>
    /// <param name="op">Operation type ("inc" or "dec").</param>
    /// <param name="value">Amount to add or subtract.</param>
    let processInstruction(registers: Dictionary<string, int>) (reg: string, op: string, value: int) =
        if not (registers.ContainsKey(reg)) then registers[reg] <- 0
        match op with
        | "inc" -> registers[reg] <- registers[reg] + value
        | "dec" -> registers[reg] <- registers[reg] - value
        | _ -> ()

    /// <summary>
    /// Processes all instructions sequentially and tracks the highest value ever held in any register.
    /// </summary>
    /// <param name="instructions">Array of instruction strings.</param>
    /// <returns>Tuple containing final register dictionary and the maximum value encountered during execution.</returns>
    let processInstructions(instructions: string[]) =
        let registers = Dictionary<string, int>()
        let mutable highestValueEver = Int32.MinValue
        for instruction in instructions do
            let reg, op, value, condReg, condOp, condVal = parseInstruction instruction
            if evaluateCondition registers (condReg, condOp, condVal) then
                processInstruction registers (reg, op, value)
                highestValueEver <- max highestValueEver (if registers.ContainsKey(reg) then registers[reg] else 0)
        registers, highestValueEver

    /// <summary>
    /// Finds the maximum value across all registers at the end of execution.
    /// </summary>
    /// <param name="registers">Dictionary of register states.</param>
    /// <returns>The highest register value, or 0 if empty.</returns>
    let findMaxValue(registers: Dictionary<string, int>) =
        if registers.Count = 0 then 0
        else registers.Values |> Seq.max

    /// <summary>
    /// Solves Part 1: finds the largest value in any register after processing all instructions.
    /// </summary>
    /// <returns>Largest final register value for Part 1.</returns>
    let solvePartOne() =
        let registers, _ = processInstructions Data
        findMaxValue registers

    /// <summary>
    /// Solves Part 2: finds the highest value held in any register during execution.
    /// </summary>
    /// <returns>Highest historical register value for Part 2.</returns>
    let solvePartTwo() =
        let _, highestValueEver = processInstructions Data
        highestValueEver

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"