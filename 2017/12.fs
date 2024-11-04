namespace _2017

open System
open System.Collections.Generic

module _12 =
    let Data = (Utils.GetInputData 12).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    let buildGraph(data: string[]) =
        let graph = Dictionary<int, List<int>>()
        for line in data do
            let parts = line.Split(" <-> ", StringSplitOptions.RemoveEmptyEntries)
            let fromNode = int parts[0]
            let toNodes = parts[1].Split(',') |> Array.map int
            if not (graph.ContainsKey(fromNode)) then graph[fromNode] <- List<int>()
            for toNode in toNodes do
                graph[fromNode].Add(toNode)
                if not (graph.ContainsKey(toNode)) then graph[toNode] <- List<int>()
                graph[toNode].Add(fromNode)
        graph

    let dfs (graph: Dictionary<int, List<int>>) (visited: HashSet<int>) startNode =
        let rec visit(node: int) =
            if not (visited.Contains(node)) then
                visited.Add(node) |> ignore
                for neighbor in graph[node] do visit neighbor
        visit startNode

    let findGroupSize graph startNode =
        let visited = HashSet<int>()
        dfs graph visited startNode
        visited.Count

    let countGroups(graph: Dictionary<int, List<int>>) =
        let visited = HashSet<int>()
        graph.Keys |> Seq.fold(fun groupCount node ->
            if not (visited.Contains(node)) then
                dfs graph visited node
                groupCount + 1
            else groupCount
        ) 0

    let solvePartOne graph = findGroupSize graph 0

    let solvePartTwo graph = countGroups graph

    let Run() =
        let graph = buildGraph Data
        printfn $"Part One: {solvePartOne graph}"
        printfn $"Part Two: {solvePartTwo graph}"