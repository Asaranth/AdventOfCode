namespace _2017

open System

module _04 =
    let Data = (Utils.GetInputData 4).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    let isValid(anagramFree: bool) (passphrase: string) =
        let words = passphrase.Split(' ')
        let normalizedWords =
            if anagramFree then
                words |> Array.map (fun word -> word.ToCharArray() |> Array.sort |> String)
            else
                words
        let uniqueWords = normalizedWords |> Set.ofArray
        Array.length normalizedWords = Set.count uniqueWords

    let solvePartOne() = Data |> Array.filter (isValid false) |> Array.length

    let solvePartTwo() = Data |> Array.filter (isValid true) |> Array.length

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"