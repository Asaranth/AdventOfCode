namespace _2017

open System

module _19 =
    let Data = (Utils.GetInputData 19).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    let findStartPosition(diagram: string[]) = diagram[0].IndexOf('|')

    let isLetter(c: char) = Char.IsLetter(c)

    let traversePath(diagram: string[]) (onVisit: int -> int -> char -> unit) =
        let rows = diagram.Length
        let cols = diagram[0].Length
        let mutable x = findStartPosition diagram
        let mutable y = 0
        let mutable direction = (0, 1)
        let mutable continueWalking = true
        onVisit x y diagram[y].[x]

        while continueWalking do
            let dx, dy = direction
            x <- x + dx
            y <- y + dy
            if x < 0 || y < 0 || x >= cols || y >= rows || diagram[y].[x] = ' ' then continueWalking <- false
            else
                let charAtPos = diagram[y][x]
                onVisit x y charAtPos
                if charAtPos = '+' then
                    direction <-
                        if dx <> 0 then
                            if y > 0 && diagram[y - 1][x] <> ' ' then (0, -1)
                            else (0, 1)
                        else
                            if x > 0 && diagram[y][x - 1] <> ' ' then (-1, 0)
                            else (1, 0)

    let solvePartOne() =
        let letters = System.Collections.Generic.List<char>()
        let onVisit _ _ charAtPos = if isLetter charAtPos then letters.Add charAtPos
        traversePath Data onVisit
        new string (letters.ToArray())

    let solvePartTwo() =
        let mutable steps = 0
        let onVisit _ _ _ = steps <- steps + 1
        traversePath Data onVisit
        steps

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"