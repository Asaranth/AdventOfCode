namespace _2017

/// <summary>
/// Day 10: Knot Hash
///
/// Implements the Knot Hash algorithm involving cyclic sublist reversals and sparse-to-dense hash reduction.
/// </summary>
module _10 =
    let Data = (Utils.GetInputData 10).Split(',') |> Array.map int

    /// <summary>
    /// Converts a character string to an array of ASCII byte values.
    /// </summary>
    /// <param name="input">The input string to convert.</param>
    /// <returns>Array of ASCII integer codes.</returns>
    let toAscii(input: string) : int[] = input |> Seq.map int |> Seq.toArray

    let standardSuffix = [|17; 31; 73; 47; 23|]

    /// <summary>
    /// Reverses a contiguous circular sublist within an array.
    /// </summary>
    /// <param name="lst">The circular list array.</param>
    /// <param name="start">Starting index of the slice.</param>
    /// <param name="length">Length of the slice to reverse.</param>
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

    /// <summary>
    /// Computes the 32-character hexadecimal knot hash for a given input string.
    /// </summary>
    /// <param name="input">Input string to hash.</param>
    /// <returns>Hexadecimal knot hash string.</returns>
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

    /// <summary>
    /// Solves Part 1: performs a single round of knot hashing on the input numbers and returns the product of the first two elements.
    /// </summary>
    /// <returns>Product of the first two numbers in the list.</returns>
    let solvePartOne() =
        let lst = [|0..255|]
        let mutable currentPosition = 0
        let mutable skipSize = 0
        for length in Data do
            reverseSublist lst currentPosition length
            currentPosition <- (currentPosition + length + skipSize) % Array.length lst
            skipSize <- skipSize + 1
        lst[0] * lst[1]

    /// <summary>
    /// Solves Part 2: computes the complete 256-element dense knot hash formatted as a hex string.
    /// </summary>
    /// <returns>32-character hex knot hash for Part 2.</returns>
    let solvePartTwo() =
        let input = String.concat "," (Array.map string Data)
        knotHash input

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"