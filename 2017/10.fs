namespace _2017

module _10 =
    let Data = (Utils.GetInputData 10).Split(',') |> Array.map int

    let toAscii(input: string) : int[] = input |> Seq.map int |> Seq.toArray

    let standardSuffix = [|17; 31; 73; 47; 23|]

    let reverseSublist(lst: int[]) start length =
        let len = Array.length lst
        let endPos = (start + length - 1) % len
        let numSwaps = length / 2
        for i in 0 .. numSwaps - 1 do
            let a = (start + i) % len
            let b = (endPos - i + len) % len
            let temp = lst[a]
            lst[a] <- lst[b]
            lst[b] <- temp

    let knotHash(input: string) : string =
        let lengths = Array.append(toAscii input) standardSuffix
        let lst = [|0..255|]
        let rounds = 64
        let mutable currentPosition = 0
        let mutable skipSize = 0
        for _ in 1..rounds do
            for length in lengths do
                reverseSublist lst currentPosition length
                currentPosition <- (currentPosition + length + skipSize) % Array.length lst
                skipSize <- skipSize + 1
        let denseHash = [|0..15|] |> Array.map(fun i -> [|0..15|] |> Array.map(fun j -> lst[i * 16 + j]) |> Array.reduce(^^^))
        denseHash |> Array.map(_.ToString("x2")) |> String.concat ""

    let solvePartOne() =
        let lst = [|0..255|]
        let mutable currentPosition = 0
        let mutable skipSize = 0
        for length in Data do
            reverseSublist lst currentPosition length
            currentPosition <- (currentPosition + length + skipSize) % Array.length lst
            skipSize <- skipSize + 1
        lst[0] * lst[1]

    let solvePartTwo() =
        let input = String.concat "," (Array.map string Data)
        knotHash input

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"