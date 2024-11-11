namespace _2017

open System
open System.Collections.Generic

module _22 =
    let Data = (Utils.GetInputData 22).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    type Direction = Up | Right | Down | Left
    type NodeState = Clean | Weakened | Infected | Flagged

    let turnLeft direction =
        match direction with
        | Up -> Left
        | Right -> Up
        | Down -> Right
        | Left -> Down

    let turnRight direction =
        match direction with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    let reverse direction =
        match direction with
        | Up -> Down
        | Right -> Left
        | Down -> Up
        | Left -> Right

    let move (x, y) direction =
        match direction with
        | Up -> (x, y - 1)
        | Right -> (x + 1, y)
        | Down -> (x, y + 1)
        | Left -> (x - 1, y)

    let solvePartOne() =
        let grid = Dictionary<int * int, bool>()
        for y in 0 .. Data.Length - 1 do
            for x in 0 .. Data[y].Length - 1 do
                let infected = Data[y].[x] = '#'
                if infected then grid[(x, y)] <- true
        let middle = Data.Length / 2
        let mutable position = (middle, middle)
        let mutable direction = Up
        let mutable infections = 0
        for _ in 1 .. 10000 do
            let isInfected = grid.ContainsKey(position) && grid[position]
            direction <- if isInfected then turnRight direction else turnLeft direction
            if isInfected then grid[position] <- false
            else
                grid[position] <- true
                infections <- infections + 1
            position <- move position direction
        infections

    let solvePartTwo() =
        let grid = Dictionary<int * int, NodeState>()
        for y in 0 .. Data.Length - 1 do
            for x in 0 .. Data[y].Length - 1 do
                let state = if Data[y].[x] = '#' then Infected else Clean
                grid[(x, y)] <- state
        let middle = Data.Length / 2
        let mutable position = (middle, middle)
        let mutable direction = Up
        let mutable infections = 0
        for _ in 1 .. 10000000 do
            let state =
                if grid.ContainsKey(position) then grid[position]
                else Clean
            direction <-
                match state with
                | Clean -> turnLeft direction
                | Weakened -> direction
                | Infected -> turnRight direction
                | Flagged -> reverse direction
            match state with
            | Clean -> grid[position] <- Weakened
            | Weakened ->
                grid[position] <- Infected
                infections <- infections + 1
            | Infected -> grid[position] <- Flagged
            | Flagged -> grid[position] <- Clean
            position <- move position direction
        infections

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"