namespace _2017

open System

/// <summary>
/// Day 04: High-Entropy Passphrases
///
/// Validates security passphrases by ensuring word uniqueness and absence of anagrams.
/// </summary>
module _04 =
    let Data = (Utils.GetInputData 4).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Checks whether a passphrase is valid under duplicate or anagram constraints.
    /// </summary>
    /// <param name="anagramFree">If true, words are sorted character-wise to detect anagram duplicates.</param>
    /// <param name="passphrase">The space-separated passphrase string.</param>
    /// <returns>True if the passphrase contains no invalid duplicate words; false otherwise.</returns>
    let isValid(anagramFree: bool) (passphrase: string) =
        let words = passphrase.Split(' ')
        let normalizedWords =
            if anagramFree then
                words |> Array.map (fun word -> word.ToCharArray() |> Array.sort |> String)
            else
                words
        let uniqueWords = normalizedWords |> Set.ofArray
        Array.length normalizedWords = Set.count uniqueWords

    /// <summary>
    /// Solves Part 1: counts passphrases containing no duplicate words.
    /// </summary>
    /// <returns>Count of valid passphrases for Part 1.</returns>
    let solvePartOne() = Data |> Array.filter (isValid false) |> Array.length

    /// <summary>
    /// Solves Part 2: counts passphrases containing no anagrams.
    /// </summary>
    /// <returns>Count of valid passphrases for Part 2.</returns>
    let solvePartTwo() = Data |> Array.filter (isValid true) |> Array.length

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"