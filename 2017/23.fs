namespace _2017

open System

/// <summary>
/// Day 23: Coprocessor Conflagration
///
/// Interprets assembly instructions and counts composite numbers across an arithmetic sequence for optimised execution.
/// </summary>
module _23 =
    let Data = (Utils.GetInputData 23).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Operand source: either a register character identifier or an immediate 64-bit value.
    /// </summary>
    type Source = Reg of char | Value of int64

    /// <summary>
    /// Coprocessor assembly instructions.
    /// </summary>
    type Inst =
        | Set of reg: char * value: Source
        | Sub of reg: char * value: Source
        | Mul of reg: char * value: Source
        | Jnz of reg: char * value: Source
        | Jump of value: Source

    /// <summary>
    /// Look up register value from map, defaulting to 0L.
    /// </summary>
    /// <param name="reg">Register character identifier.</param>
    /// <param name="registers">Current register map.</param>
    /// <returns>64-bit integer value in the register.</returns>
    let regVal reg registers = Map.tryFind reg registers |> Option.defaultValue 0L

    /// <summary>
    /// Parses an operand token as either a register name or integer value.
    /// </summary>
    /// <param name="text">Token text.</param>
    /// <returns>Source representation.</returns>
    let getSource(text: string) = if Char.IsLetter text[0] then Reg text[0] else Value (int64 text)

    /// <summary>
    /// Evaluates a <see cref="Source"/> operand against current register map.
    /// </summary>
    /// <param name="registers">Current register map.</param>
    /// <param name="source">Source operand.</param>
    /// <returns>Resolved 64-bit integer.</returns>
    let getSourceValue registers = function | Value n -> n | Reg c -> regVal c registers

    /// <summary>
    /// Parsed array of instructions for the coprocessor.
    /// </summary>
    let instructions =
        Data
        |> Array.map (fun line ->
            match line.Split(' ') with
            | [| "set"; reg; regOrVal |] -> Set (reg[0], getSource regOrVal)
            | [| "sub"; reg; regOrVal |] -> Sub (reg[0], getSource regOrVal)
            | [| "mul"; reg; regOrVal |] -> Mul (reg[0], getSource regOrVal)
            | [| "jnz"; test; regOrVal |] ->
                match getSource test with
                | Reg r -> Jnz (r, getSource regOrVal)
                | Value v -> if v <> 0L then Jump (getSource regOrVal) else Jump (Value 1L)
            | _ -> failwith "unrecognised instruction")

    /// <summary>
    /// Solves Part 1: counts the number of times the 'mul' instruction is invoked during unoptimised execution.
    /// </summary>
    /// <returns>Total multiplication count for Part 1.</returns>
    let solvePartOne() =
        let rec processor index registers mulCount =
            if index < 0 || index >= instructions.Length then mulCount
            else
                match instructions[index] with
                | Jump amount -> processor (index + int (getSourceValue registers amount)) registers mulCount
                | Jnz (register, amount) ->
                    if regVal register registers = 0L then processor (index + 1) registers mulCount
                    else processor (index + int (getSourceValue registers amount)) registers mulCount
                | Set (target, source) ->
                    let registers = Map.add target (getSourceValue registers source) registers
                    processor (index + 1) registers mulCount
                | Sub (target, source) ->
                    let newVal = regVal target registers - getSourceValue registers source
                    let registers = Map.add target newVal registers
                    processor (index + 1) registers mulCount
                | Mul (target, source) ->
                    let newVal = regVal target registers * getSourceValue registers source
                    let registers = Map.add target newVal registers
                    processor (index + 1) registers (mulCount + 1)
        processor 0 Map.empty 0

    /// <summary>
    /// Solves Part 2: counts non-prime (composite) numbers in the arithmetic range from 106,700 to 123,700 with step 17.
    /// </summary>
    /// <returns>Count of composite numbers stored in register h for Part 2.</returns>
    let solvePartTwo() =
        let isPrime n =
            match n with
            | _ when n > 3 && (n % 2 = 0 || n % 3 = 0) -> false
            | _ ->
                let maxDiv = int(Math.Sqrt(float n)) + 1
                let rec f d i =
                    if d > maxDiv then true
                    else
                        if n % d = 0 then false
                        else f (d + i) (6 - i)
                f 5 2
        [106700..17..123700] |> List.filter (isPrime >> not) |> List.length

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"