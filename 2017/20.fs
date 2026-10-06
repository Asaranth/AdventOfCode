namespace _2017

open System
open System.Text.RegularExpressions
open System.Collections.Generic

/// <summary>
/// Day 20: Particle Swarm
///
/// Simulates 3D particle kinematics (position, velocity, acceleration) and resolves multi-particle collisions.
/// </summary>
module _20 =
    let Data = (Utils.GetInputData 20).Split('\n', StringSplitOptions.RemoveEmptyEntries)

    /// <summary>
    /// Represents a 3D coordinate vector.
    /// </summary>
    type Vector = { X: int; Y: int; Z: int }

    /// <summary>
    /// Represents a particle with mutable 3D position (P), velocity (V), and acceleration (A) vectors.
    /// </summary>
    type Particle = { mutable P: Vector; mutable V: Vector; mutable A: Vector }

    /// <summary>
    /// Parses a vector string in the format "&lt;X,Y,Z&gt;" into a <see cref="Vector"/> record.
    /// </summary>
    /// <param name="s">Vector formatted string.</param>
    /// <returns>Parsed 3D Vector.</returns>
    let parseVector(s: string) =
        let regex = Regex(@"<(-?\d+),(-?\d+),(-?\d+)>")
        let matches = regex.Match(s)
        { X = int matches.Groups[1].Value; Y = int matches.Groups[2].Value; Z = int matches.Groups[3].Value }

    /// <summary>
    /// Parses a line containing position, velocity, and acceleration coordinates into a <see cref="Particle"/>.
    /// </summary>
    /// <param name="line">Particle specification string.</param>
    /// <returns>Initialized Particle record.</returns>
    let parseParticle(line: string) =
        let parts = line.Split(", ", StringSplitOptions.RemoveEmptyEntries)
        { P = parseVector (parts[0].Substring(2)); V = parseVector (parts[1].Substring(2)); A = parseVector (parts[2].Substring(2)) }

    /// <summary>
    /// Calculates the Manhattan distance of a 3D vector from the origin (0, 0, 0).
    /// </summary>
    /// <param name="v">The 3D vector.</param>
    /// <returns>Manhattan distance sum.</returns>
    let manhattanDistance(v: Vector) = Math.Abs(v.X) + Math.Abs(v.Y) + Math.Abs(v.Z)

    /// <summary>
    /// Updates a particle's velocity by its acceleration and position by its updated velocity for one tick.
    /// </summary>
    /// <param name="p">Particle to update in place.</param>
    let updateParticle(p: Particle) =
        p.V <- { X = p.V.X + p.A.X; Y = p.V.Y + p.A.Y; Z = p.V.Z + p.A.Z }
        p.P <- { X = p.P.X + p.V.X; Y = p.P.Y + p.V.Y; Z = p.P.Z + p.V.Z }

    /// <summary>
    /// Solves Part 1: finds the particle that stays closest to the origin in the long term (smallest acceleration).
    /// </summary>
    /// <returns>0-based index of the closest particle.</returns>
    let solvePartOne() =
        let particles = Data |> Array.map parseParticle
        let closestParticle = particles |> Array.mapi(fun i p -> (i, manhattanDistance p.A)) |> Array.minBy snd
        fst closestParticle

    /// <summary>
    /// Solves Part 2: simulates particle movements and eliminates particles that collide at the same position.
    /// </summary>
    /// <returns>Count of surviving particles after collisions.</returns>
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

    /// <summary>
    /// Executes and prints the solutions for Part 1 and Part 2.
    /// </summary>
    let Run() =
        printfn $"Part One: {solvePartOne()}"
        printfn $"Part Two: {solvePartTwo()}"