namespace _2017

open System

/// <summary>
/// Day 14: Disk Defragmentation
///
/// Generates 128x128 binary memory grids from knot hashes and identifies contiguous connected used regions via BFS.
/// </summary>
module _14 =
    let Data = (Utils.GetInputData 14).Trim()
    let gridSize = 128

    /// <summary>
    /// Computes the sparse 256-element circular knot hash list for given input lengths.
    /// </summary>
    /// <param name="listSize">Size of the number sequence list (256).</param>
    /// <param name="lengths">Sequence of sublist reversal lengths including standard salt.</param>
    /// <returns>Sparse list of 256 integers after 64 rounds.</returns>
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

    /// <summary>
    /// Compresses a 256-element sparse hash into 16 dense bytes using bitwise XOR over 16-element blocks.
    /// </summary>
    /// <param name="sparseHash">Sparse list of 256 integers.</param>
    /// <returns>Dense list of 16 integers.</returns>
    let denseHash sparseHash = sparseHash |> List.chunkBySize 16 |> List.map(List.reduce(^^^))

    /// <summary>
    /// Computes the 32-character hexadecimal knot hash for a given string.
    /// </summary>
    /// <param name="input">String to hash.</param>
    /// <returns>Hexadecimal knot hash string.</returns>
    let knotHash input =
        let asciiValues = input |> Seq.map int |> Seq.toList
        let lengths = asciiValues @ [17; 31; 73; 47; 23]
        let sparse = sparseHash 256 lengths
        let dense = denseHash sparse
        dense |> List.map(_.ToString("x2")) |> String.concat ""

    /// <summary>
    /// Converts a hexadecimal string into its 4-bit binary representation.
    /// </summary>
    /// <param name="hex">Hexadecimal string.</param>
    /// <returns>String of '0' and '1' binary digits.</returns>
    let toBinary hex =
        hex
        |> Seq.map(fun c -> Convert.ToString(Convert.ToInt32(c.ToString(), 16), 2).PadLeft(4, '0'))
        |> String.concat ""

    /// <summary>
    /// Generates the 128x128 grid of binary characters from the puzzle key string.
    /// </summary>
    /// <param name="key">Puzzle input key string.</param>
    /// <returns>2D array of grid row character arrays.</returns>
    let generateGrid key =
        [0 .. gridSize - 1]
        |> List.map(fun i -> knotHash (key + "-" + i.ToString()) |> toBinary |> Seq.toArray)
        |> List.toArray

    /// <summary>
    /// Counts total number of used ('1') squares across the 128x128 grid.
    /// </summary>
    /// <param name="key">Puzzle input key string.</param>
    /// <returns>Total count of used squares.</returns>
    let countUsedSquares key =
        generateGrid key |> Array.sumBy(fun row -> row |> Seq.filter (fun c -> c = '1') |> Seq.length)

    /// <summary>
    /// Performs a breadth-first search to traverse and mark all orthogonally connected used squares in a region.
    /// </summary>
    /// <param name="grid">The 128x128 grid characters.</param>
    /// <param name="visited">Boolean matrix of visited coordinates.</param>
    /// <param name="x">Starting x-coordinate.</param>
    /// <param name="y">Starting y-coordinate.</param>
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

    /// <summary>
    /// Counts the total number of connected orthogonal regions of used squares on the disk.
    /// </summary>
    /// <param name="key">Puzzle input key string.</param>
    /// <returns>Total number of distinct regions.</returns>
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

    /// <summary>
    /// Solves Part 1: counts the number of used squares in the 128x128 grid.
    /// </summary>
    /// <returns>Used square count for Part 1.</returns>
    let solvePartOne() = countUsedSquares Data

    /// <summary>
    /// Solves Part 2: counts the number of contiguous connected regions.
    /// </summary>
    /// <returns>Region count for Part 2.</returns>
    let solvePartTwo() = countRegions Data

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"