namespace _2017

module _03 =
    let Data = (Utils.GetInputData 3).Trim() |> int

    let findManhattanDistance input =
        let rec findLayerAndMaxValue layer value =
            if value * value >= input then (layer, value)
            else findLayerAndMaxValue (layer + 1) (value + 2)
        let layer, value = findLayerAndMaxValue 0 1
        let maxInLayer = value * value
        let sideLength = value - 1
        let middlePoints = [
            maxInLayer - sideLength / 2
            maxInLayer - (sideLength * 3 / 2)
            maxInLayer - (sideLength * 5 / 2)
            maxInLayer - (sideLength * 7 / 2)
        ]
        let distanceToMiddleOfEdge = middlePoints |> List.map(fun middle -> abs(input - middle)) |> List.min
        layer + distanceToMiddleOfEdge

    let solvePartOne() = findManhattanDistance Data

    let solvePartTwo() =
        let directions = [(0, 1); (1, 0); (0, -1); (-1, 0); (1, 1); (-1, -1); (1, -1); (-1, 1)]
        let getValue grid (x, y) = Map.tryFind (x, y) grid |> Option.defaultValue 0
        let rec findValue grid x y value (dx, dy) =
            let surroundingSum = directions |> List.sumBy (fun (dx, dy) -> getValue grid (x + dx, y + dy))
            let newGrid = Map.add (x, y) surroundingSum grid
            if surroundingSum > value then surroundingSum
            else
                let newX, newY, newDir =
                    match (dx, dy) with
                    | 1, 0 -> if getValue newGrid (x, y + 1) = 0 then (x, y + 1, (0, 1)) else (x + 1, y, (1, 0))
                    | 0, 1 -> if getValue newGrid (x - 1, y) = 0 then (x - 1, y, (-1, 0)) else (x, y + 1, (0, 1))
                    | -1, 0 -> if getValue newGrid (x, y - 1) = 0 then (x, y - 1, (0, -1)) else (x - 1, y, (-1, 0))
                    | 0, -1 -> if getValue newGrid (x + 1, y) = 0 then (x + 1, y, (1, 0)) else (x, y - 1, (0, -1))
                    | _ -> failwith "Unexpected direction"
                findValue newGrid newX newY value newDir
        let initialGrid = Map.empty |> Map.add (0, 0) 1
        findValue initialGrid 1 0 Data (1, 0)

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"