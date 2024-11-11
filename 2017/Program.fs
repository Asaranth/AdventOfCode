namespace _2017

open System

module Program =
    let solutions = dict [
        (1, _01.Run); (2, _02.Run); (3, _03.Run); (4, _04.Run); (5, _05.Run); (6, _06.Run); (7, _07.Run); (8, _08.Run)
        (9, _09.Run); (10, _10.Run); (11, _11.Run); (12, _12.Run); (13, _13.Run); (14, _14.Run); (15, _15.Run)
        (16, _16.Run); (17, _17.Run); (18, _18.Run); (19, _19.Run);
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