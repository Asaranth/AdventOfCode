namespace _2017

open System
open System.Text.RegularExpressions
open System.Collections.Generic

module _20 =
    let Data = (Utils.GetInputData 20).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    type Vector = { X: int; Y: int; Z: int }

    type Particle = { mutable P: Vector; mutable V: Vector; mutable A: Vector }

    let parseVector(s: string) =
        let regex = Regex(@"<(-?\d+),(-?\d+),(-?\d+)>")
        let matches = regex.Match(s)
        { X = int matches.Groups[1].Value; Y = int matches.Groups[2].Value; Z = int matches.Groups[3].Value }

    let parseParticle(line: string) =
        let parts = line.Split(", ", StringSplitOptions.RemoveEmptyEntries)
        { P = parseVector (parts[0].Substring(2)); V = parseVector (parts[1].Substring(2)); A = parseVector (parts[2].Substring(2)) }

    let manhattanDistance(v: Vector) = Math.Abs(v.X) + Math.Abs(v.Y) + Math.Abs(v.Z)

    let updateParticle(p: Particle) =
        p.V <- { X = p.V.X + p.A.X; Y = p.V.Y + p.A.Y; Z = p.V.Z + p.A.Z }
        p.P <- { X = p.P.X + p.V.X; Y = p.P.Y + p.V.Y; Z = p.P.Z + p.V.Z }

    let solvePartOne() =
        let particles = Data |> Array.map parseParticle
        let closestParticle = particles |> Array.mapi(fun i p -> (i, manhattanDistance p.A)) |> Array.minBy snd
        fst closestParticle

    let solvePartTwo() =
        let particles = Data |> Array.map parseParticle |> List.ofArray
        let mutable activeParticles = particles
        for step in 1..1000 do
            activeParticles |> List.iter updateParticle
            let groups = activeParticles |> List.groupBy(_.P) |> List.filter(fun (_, ps) -> List.length ps > 1)
            let collidedParticles = HashSet()
            for _, ps in groups do ps |> List.iter(fun p -> collidedParticles.Add p |> ignore)
            activeParticles <- activeParticles |> List.filter(fun p -> not (collidedParticles.Contains p))
        activeParticles.Length


    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"