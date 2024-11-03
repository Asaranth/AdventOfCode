namespace _2017

open System

module Program =
    let solutions = dict [
        1, _01.Run
        2, _02.Run
        3, _03.Run
    ]

    let runSolution dayNumber =
        match solutions.TryGetValue(dayNumber) with
        | true, runAction -> runAction()
        | false, _ -> printfn "Solution for the given day is not implemented yet."

    [<EntryPoint>]
    let main _ =
        Console.Write("Enter the day number you want to run (1-25): ")
        let input = Console.ReadLine()
        match Int32.TryParse(input) with
        | true, dayNumber when dayNumber >= 1 && dayNumber <= 25 -> runSolution dayNumber
        | _ -> printfn "Invalid input. Please enter a number between 1 and 25"
        0