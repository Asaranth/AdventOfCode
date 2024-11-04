namespace _2017

open System

module _14 =
    let Data = (Utils.GetInputData 14).Trim()
    let gridSize = 128

    let sparseHash listSize lengths =
        let mutable list = [| 0 .. listSize - 1 |]
        let mutable currentPosition = 0
        let mutable skipSize = 0
        for _ in 0 .. 63 do
            for length in lengths do
                let sublist = [ for i in 0 .. length - 1 -> list[(currentPosition + i) % listSize] ] |> List.rev
                for i in 0 .. length - 1 do list[(currentPosition + i) % listSize] <- sublist[i]
                currentPosition <- (currentPosition + length + skipSize) % listSize
                skipSize <- skipSize + 1
        list |> Array.toList

    let denseHash sparseHash = sparseHash |> List.chunkBySize 16 |> List.map(List.reduce(^^^))

    let knotHash input =
        let asciiValues = input |> Seq.map int |> Seq.toList
        let lengths = asciiValues @ [17; 31; 73; 47; 23]
        let sparse = sparseHash 256 lengths
        let dense = denseHash sparse
        dense |> List.map(_.ToString("x2")) |> String.concat ""

    let toBinary hex =
        hex
        |> Seq.map(fun c -> Convert.ToString(Convert.ToInt32(c.ToString(), 16), 2).PadLeft(4, '0'))
        |> String.concat ""

    let generateGrid key =
        [0 .. gridSize - 1]
        |> List.map(fun i -> knotHash (key + "-" + i.ToString()) |> toBinary |> Seq.toArray)
        |> List.toArray

    let countUsedSquares key =
        generateGrid key |> Array.sumBy(fun row -> row |> Seq.filter (fun c -> c = '1') |> Seq.length)

    let bfs (grid: char[][]) (visited: bool[][]) (x, y) =
        let directions = [(-1, 0); (1, 0); (0, -1); (0, 1)]
        let mutable queue = [(x, y)]
        visited[x].[y] <- true
        while queue <> [] do
            let cx, cy = List.head queue
            queue <- List.tail queue
            for dx, dy in directions do
                let nx, ny = cx + dx, cy + dy
                if nx >= 0 && ny >= 0 && nx < gridSize && ny < gridSize then
                    if grid[nx].[ny] = '1' && not visited[nx].[ny] then
                        visited[nx].[ny] <- true
                        queue <- (nx, ny) :: queue

    let countRegions key =
        let grid = generateGrid key
        let visited = Array.init gridSize (fun _ -> Array.create gridSize false)
        let mutable regionCount = 0
        for x in 0 .. gridSize - 1 do
            for y in 0 .. gridSize - 1 do
                if grid[x].[y] = '1' && not visited[x].[y] then
                    regionCount <- regionCount + 1
                    bfs grid visited (x, y)
        regionCount

    let solvePartOne() = countUsedSquares Data

    let solvePartTwo() = countRegions Data

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"