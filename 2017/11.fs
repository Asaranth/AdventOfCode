namespace _2017

open System

module _11 =
    let Data = (Utils.GetInputData 11).Split(',', StringSplitOptions.RemoveEmptyEntries) |> Array.map(_.Trim())

    let directions =
        dict [
            "n",  (0, -1)
            "ne", (1, -1)
            "se", (1, 0)
            "s",  (0, 1)
            "sw", (-1, 1)
            "nw", (-1, 0)
        ]

    let add (q1, r1) (q2, r2) = (q1 + q2, r1 + r2)

    let distance (q1, r1) (q2, r2) = (abs (q1 - q2) + abs ((q1 + r1) - (q2 + r2)) + abs (r1 - r2)) / 2

    let solve(isPartTwo: bool) =
        let finalPosition, maxDistance =
            Data |> Array.fold (fun (acc, maxDist) dir ->
                let newPos = add acc directions[dir]
                newPos, max maxDist (distance (0, 0) newPos)
            ) ((0, 0), 0)
        if isPartTwo then maxDistance else distance (0, 0) finalPosition


    let solvePartOne() = solve false

    let solvePartTwo() = solve true

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"