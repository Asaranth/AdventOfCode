namespace _2017

open System
open System.IO
open System.Net.Http
open Microsoft.Extensions.Configuration

/// <summary>
/// Common Utilities
///
/// Provides HTTP client utilities and puzzle input caching.
/// </summary>
module Utils =
    let httpClient = new HttpClient()

    /// <summary>
    /// Retrieves the session cookie from application configuration.
    /// </summary>
    /// <returns>The session cookie string.</returns>
    let getSessionCookie() =
        let configurationBuilder = ConfigurationBuilder().SetBasePath(AppContext.BaseDirectory).AddJsonFile("appsettings.json", optional = false, reloadOnChange = true)

        let configuration = configurationBuilder.Build()
        configuration["AOC_SESSION_COOKIE"]

    /// <summary>
    /// Asynchronously fetches puzzle input for a given day, caching the response locally.
    /// </summary>
    /// <param name="day">Day number of the puzzle.</param>
    /// <returns>Raw puzzle input text.</returns>
    let getPuzzleInput day =async {
        let cacheFilePath = Path.Combine("data", $"{day:D2}.txt")
        if File.Exists(cacheFilePath) then return File.ReadAllText(cacheFilePath)
        else
            let sessionCookie = getSessionCookie()
            let url = $"https://adventofcode.com/2017/day/{day}/input"
            httpClient.DefaultRequestHeaders.Clear()
            httpClient.DefaultRequestHeaders.Add("Cookie", $"session={sessionCookie}")
            let! response = httpClient.GetStringAsync(url) |> Async.AwaitTask
            if not (Directory.Exists("data")) then Directory.CreateDirectory("data") |> ignore
            File.WriteAllText(cacheFilePath, response)
            return response
    }

    /// <summary>
    /// Synchronously retrieves puzzle input data for a given day.
    /// </summary>
    /// <param name="day">Day number of the puzzle.</param>
    /// <returns>Puzzle input as a string.</returns>
    let GetInputData day = getPuzzleInput day |> Async.RunSynchronously