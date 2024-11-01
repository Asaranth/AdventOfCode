namespace _2017

module _01 =
    let Data = (Utils.GetInputData 1).Trim()

    let getDigits() = Data |> Seq.map(fun c -> int c - int '0') |> Seq.toArray

    let calculateSum digits len offset =
        digits |> Array.mapi(fun i digit -> if digit = digits[(i + offset) % len] then digit else 0) |> Array.sum

    let solvePartOne() =
        let digits = getDigits()
        calculateSum digits digits.Length 1

    let solvePartTwo() =
        let digits = getDigits()
        calculateSum digits digits.Length (digits.Length / 2)

    let Run () =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"