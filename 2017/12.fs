namespace _2017

open System
open System.Collections.Generic

/// <summary>
/// Day 12: Digital Plumber
///
/// Constructs bidirectional pipe communication graphs and determines connected component group sizes.
/// </summary>
module _12 =
    let Data = (Utils.GetInputData 12).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Builds an adjacency list graph representing bidirectional communication pipes between program IDs.
    /// </summary>
    /// <param name="data">Array of pipe connection lines.</param>
    /// <returns>Dictionary mapping each program ID to a list of connected neighbour IDs.</returns>
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

    /// <summary>
    /// Performs a depth-first traversal to discover all connected nodes in the graph.
    /// </summary>
    /// <param name="graph">The adjacency list graph.</param>
    /// <param name="visited">Set of visited program IDs.</param>
    /// <param name="startNode">Starting node ID for the traversal.</param>
    let dfs (graph: Dictionary<int, List<int>>) (visited: HashSet<int>) startNode =
        let rec visit(node: int) =
            if not (visited.Contains(node)) then
                visited.Add(node) |> ignore
                for neighbor in graph[node] do visit neighbor
        visit startNode

    /// <summary>
    /// Finds the total number of programs in the connected component containing <paramref name="startNode"/>.
    /// </summary>
    /// <param name="graph">The adjacency list graph.</param>
    /// <param name="startNode">Starting program ID.</param>
    /// <returns>Count of programs in the group.</returns>
    let findGroupSize graph startNode =
        let visited = HashSet<int>()
        dfs graph visited startNode
        visited.Count

    /// <summary>
    /// Counts the total number of disjoint connected component groups in the network.
    /// </summary>
    /// <param name="graph">The adjacency list graph.</param>
    /// <returns>Number of independent groups.</returns>
    let countGroups(graph: Dictionary<int, List<int>>) =
        let visited = HashSet<int>()
        graph.Keys |> Seq.fold(fun groupCount node ->
            if not (visited.Contains(node)) then
                dfs graph visited node
                groupCount + 1
            else groupCount
        ) 0

    /// <summary>
    /// Solves Part 1: finds the number of programs in the group containing program ID 0.
    /// </summary>
    /// <param name="graph">The adjacency list graph.</param>
    /// <returns>Size of group containing program 0.</returns>
    let solvePartOne graph = findGroupSize graph 0

    /// <summary>
    /// Solves Part 2: counts the total number of disconnected groups across all programs.
    /// </summary>
    /// <param name="graph">The adjacency list graph.</param>
    /// <returns>Total number of groups for Part 2.</returns>
    let solvePartTwo graph = countGroups graph

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        let graph = buildGraph Data
        printfn $"Part One: {solvePartOne graph}"
        printfn $"Part Two: {solvePartTwo graph}"