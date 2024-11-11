namespace _2017

open System

module _21 =
    let Data = (Utils.GetInputData 21).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    let rotate(grid: string[]) =
        let size = grid.Length
        [| for x in 0 .. size - 1 -> String([| for y in (size - 1) .. -1 .. 0 -> grid[y][x] |]) |]

    let flip(grid: string[]) = [| for row in grid -> String(row.ToCharArray() |> Array.rev) |]

    let allVariations(grid: string[]) =
        let rotateOnce = rotate grid
        let rotateTwice = rotate rotateOnce
        let rotateThrice = rotate rotateTwice
        [| grid; rotateOnce; rotateTwice; rotateThrice; flip grid; flip rotateOnce; flip rotateTwice; flip rotateThrice |]

    let parsePattern(pattern: string) = pattern.Split('/')

    let parseRule(rule: string) =
        let parts = rule.Split(" => ")
        (parsePattern parts[0], parsePattern parts[1])

    let rules = Data |> Array.map parseRule |> dict

    let matchRule(square: string[]) =
        allVariations square |> Array.tryPick(fun variant -> if rules.ContainsKey(variant) then Some(rules[variant]) else None)

    let breakIntoSquares(grid: string[]) size =
        let n = grid.Length / size
        [| for y in 0 .. n - 1 do
               for x in 0 .. n - 1 do
                   yield [| for row in grid[(y * size) .. (y * size + size - 1)] -> row.Substring(x * size, size) |] |]

    let combineSquares(squares: string[][]) originalSize newSize =
        let blockDim = originalSize / newSize
        let resultSize = blockDim * (newSize + 1)
        Array.init resultSize (fun y ->
            let blockRow = y / (newSize + 1)
            let inRow = y % (newSize + 1)
            Array.init blockDim (fun x -> squares[blockRow * blockDim + x][inRow]) |> String.concat "")

    let enhanceGrid(grid: string[]) =
        let size = if grid.Length % 2 = 0 then 2 else 3
        let squares = breakIntoSquares grid size
        let enhancedSquares = squares |> Array.map matchRule |> Array.choose id
        combineSquares enhancedSquares grid.Length size

    let rec enhance grid iterations = if iterations = 0 then grid else enhance (enhanceGrid grid) (iterations - 1)

    let solvePartOne() =
        let initialGrid = [|".#."; "..#"; "###"|]
        let finalGrid = enhance initialGrid 5
        finalGrid |> Array.sumBy(fun row -> row |> String.filter (fun c -> c = '#') |> String.length)

    let solvePartTwo () =
        let initialGrid = [|".#."; "..#"; "###"|]
        let finalGrid = enhance initialGrid 18
        finalGrid |> Array.sumBy(fun row -> row |> String.filter (fun c -> c = '#') |> String.length)

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"