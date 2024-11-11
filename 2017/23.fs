namespace _2017

open System
open System.Collections.Generic

module _23 =

    let Data = (Utils.GetInputData 23).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    type Source = Reg of char | Value of int64
    type Inst =
        | Set of reg: char * value: Source
        | Sub of reg: char * value: Source
        | Mul of reg: char * value: Source
        | Jnz of reg: char * value: Source
        | Jump of value: Source

    let regVal reg registers =
        Map.tryFind reg registers |> Option.defaultValue 0L

    let getSource(text: string) = if Char.IsLetter text.[0] then Reg text[0] else Value (int64 text)

    let getSourceValue registers = function | Value n -> n | Reg c -> regVal c registers

    let instructions =
        Data
        |> Array.map (fun line ->
            match line.Split(' ') with
            | [| "set"; reg; regOrVal |] -> Set (reg.[0], getSource regOrVal)
            | [| "sub"; reg; regOrVal |] -> Sub (reg.[0], getSource regOrVal)
            | [| "mul"; reg; regOrVal |] -> Mul (reg.[0], getSource regOrVal)
            | [| "jnz"; test; regOrVal |] ->
                match getSource test with
                | Reg r -> Jnz (r, getSource regOrVal)
                | Value v -> if v <> 0L then Jump (getSource regOrVal) else Jump (Value 1L)
            | _ -> failwith "unrecognised instruction")

    let solvePartOne() =
        let rec processor index registers mulCount =
            if index < 0 || index >= instructions.Length then mulCount
            else
                match instructions.[index] with
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

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"