namespace _2017

open System

/// <summary>
/// Day 21: Fractal Art
///
/// Expands image pixel patterns through square subdivision, rotational and reflectional symmetry matching, and rule replacement.
/// </summary>
module _21 =
    let Data = (Utils.GetInputData 21).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Rotates a square grid 90 degrees clockwise.
    /// </summary>
    /// <param name="grid">Square grid represented as array of strings.</param>
    /// <returns>Rotated grid.</returns>
    let rotate(grid: string[]) =
        let size = grid.Length
        [| for x in 0 .. size - 1 -> String([| for y in (size - 1) .. -1 .. 0 -> grid[y][x] |]) |]

    /// <summary>
    /// Flips a square grid horizontally.
    /// </summary>
    /// <param name="grid">Square grid represented as array of strings.</param>
    /// <returns>Flipped grid.</returns>
    let flip(grid: string[]) = [| for row in grid -> String(row.ToCharArray() |> Array.rev) |]

    /// <summary>
    /// Generates all 8 possible rotational and reflectional variations of a square grid.
    /// </summary>
    /// <param name="grid">Original square grid.</param>
    /// <returns>Array of 8 pattern variations.</returns>
    let allVariations(grid: string[]) =
        let rotateOnce = rotate grid
        let rotateTwice = rotate rotateOnce
        let rotateThrice = rotate rotateTwice
        [| grid; rotateOnce; rotateTwice; rotateThrice; flip grid; flip rotateOnce; flip rotateTwice; flip rotateThrice |]

    /// <summary>
    /// Parses a slash-separated pattern string into an array of row strings.
    /// </summary>
    /// <param name="pattern">Pattern string (e.g. "../.#").</param>
    /// <returns>Array of pattern row strings.</returns>
    let parsePattern(pattern: string) = pattern.Split('/')

    /// <summary>
    /// Parses an enhancement rule in the form "input =&gt; output".
    /// </summary>
    /// <param name="rule">Rule line string.</param>
    /// <returns>Tuple of (input pattern, output pattern).</returns>
    let parseRule(rule: string) =
        let parts = rule.Split(" => ")
        (parsePattern parts[0], parsePattern parts[1])

    let rules = Data |> Array.map parseRule |> dict

    /// <summary>
    /// Finds the replacement pattern for a square across all its symmetry variants.
    /// </summary>
    /// <param name="square">Square pattern to match.</param>
    /// <returns>Matching output pattern if found.</returns>
    let matchRule(square: string[]) =
        allVariations square |> Array.tryPick(fun variant -> if rules.ContainsKey(variant) then Some(rules[variant]) else None)

    /// <summary>
    /// Divides an NxN grid into sub-squares of dimension <paramref name="size"/>x<paramref name="size"/>.
    /// </summary>
    /// <param name="grid">Source grid.</param>
    /// <param name="size">Sub-square side length (2 or 3).</param>
    /// <returns>Array of sub-squares.</returns>
    let breakIntoSquares(grid: string[]) size =
        let n = grid.Length / size
        [| for y in 0 .. n - 1 do
               for x in 0 .. n - 1 do
                   yield [| for row in grid[(y * size) .. (y * size + size - 1)] -> row.Substring(x * size, size) |] |]

    /// <summary>
    /// Recombines an array of enhanced sub-squares into a single larger composite grid.
    /// </summary>
    /// <param name="squares">Array of sub-squares.</param>
    /// <param name="originalSize">Side length of the grid before expansion.</param>
    /// <param name="newSize">Sub-square size before expansion (2 or 3).</param>
    /// <returns>Combined grid array of strings.</returns>
    let combineSquares(squares: string[][]) originalSize newSize =
        let blockDim = originalSize / newSize
        let resultSize = blockDim * (newSize + 1)
        Array.init resultSize (fun y ->
            let blockRow = y / (newSize + 1)
            let inRow = y % (newSize + 1)
            Array.init blockDim (fun x -> squares[blockRow * blockDim + x][inRow]) |> String.concat "")

    /// <summary>
    /// Performs one iteration of splitting, matching rules, and recombining the grid.
    /// </summary>
    /// <param name="grid">The current grid.</param>
    /// <returns>Enhanced and expanded grid.</returns>
    let enhanceGrid(grid: string[]) =
        let size = if grid.Length % 2 = 0 then 2 else 3
        let squares = breakIntoSquares grid size
        let enhancedSquares = squares |> Array.map matchRule |> Array.choose id
        combineSquares enhancedSquares grid.Length size

    /// <summary>
    /// Applies fractal enhancement repeatedly for a given number of iterations.
    /// </summary>
    /// <param name="grid">Initial grid state.</param>
    /// <param name="iterations">Number of enhancement steps.</param>
    /// <returns>Final grid after all iterations.</returns>
    let rec enhance grid iterations = if iterations = 0 then grid else enhance (enhanceGrid grid) (iterations - 1)

    /// <summary>
    /// Solves Part 1: counts the number of pixels turned on ('#') after 5 iterations.
    /// </summary>
    /// <returns>Number of on pixels for Part 1.</returns>
    let solvePartOne() =
        let initialGrid = [|".#."; "..#"; "###"|]
        let finalGrid = enhance initialGrid 5
        finalGrid |> Array.sumBy(fun row -> row |> String.filter (fun c -> c = '#') |> String.length)

    /// <summary>
    /// Solves Part 2: counts the number of pixels turned on ('#') after 18 iterations.
    /// </summary>
    /// <returns>Number of on pixels for Part 2.</returns>
    let solvePartTwo () =
        let initialGrid = [|".#."; "..#"; "###"|]
        let finalGrid = enhance initialGrid 18
        finalGrid |> Array.sumBy(fun row -> row |> String.filter (fun c -> c = '#') |> String.length)

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"