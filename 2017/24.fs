namespace _2017

open System

module _24 =
    let Data = (Utils.GetInputData 24).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    type Component = int * int

    let parseComponents = Data |> Array.map (fun line ->
        let ports = line.Split('/')
        int ports[0], int ports[1]) |> Array.toList

    let bridgeStrength(bridge: Component list) = List.sumBy (fun (a, b) -> a + b) bridge

    let rec findStrongest(components: Component list) currentPort (currentBridge: Component list) =
        let possibleComponents = components |> List.filter (fun (a, b) -> a = currentPort || b = currentPort)

        if List.isEmpty possibleComponents then currentBridge
        else
            let allBridges = possibleComponents |> List.collect (fun c ->
                let remainingComponents = components |> List.except [c]
                let nextPort = if fst c = currentPort then snd c else fst c
                [findStrongest remainingComponents nextPort (c :: currentBridge)])

            List.maxBy bridgeStrength (currentBridge :: allBridges)

    let rec findLongestAndStrongest (components: Component list) currentPort (currentBridge: Component list) =
        let possibleComponents = components |> List.filter (fun (a, b) -> a = currentPort || b = currentPort)
        if List.isEmpty possibleComponents then (currentBridge, List.length currentBridge, bridgeStrength currentBridge)
        else
            let allBridges = possibleComponents |> List.collect (fun c ->
                let remainingComponents = components |> List.except [c]
                let nextPort = if fst c = currentPort then snd c else fst c
                let bridge, length, strength = findLongestAndStrongest remainingComponents nextPort (c :: currentBridge)
                [(bridge, length, strength)])

            let longestBridge, longestLength, strongestStrength = allBridges |> List.maxBy (fun (_, length, strength) -> length, strength)
            if longestLength > List.length currentBridge then (longestBridge, longestLength, strongestStrength)
            else (currentBridge, List.length currentBridge, bridgeStrength currentBridge)

    let solvePartOne() =
        let strongestBridge = findStrongest parseComponents 0 []
        bridgeStrength strongestBridge

    let solvePartTwo() =
        let longestAndStrongestBridge, _, _ = findLongestAndStrongest parseComponents 0 []
        bridgeStrength longestAndStrongestBridge

    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"