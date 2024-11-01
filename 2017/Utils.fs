namespace _2017

open System
open System.IO
open System.Net.Http
open Microsoft.Extensions.Configuration

module Utils =
    let httpClient = new HttpClient()

    let getSessionCookie() =
        let configurationBuilder = ConfigurationBuilder().SetBasePath(AppContext.BaseDirectory).AddJsonFile("appsettings.json", optional = false, reloadOnChange = true)

        let configuration = configurationBuilder.Build()
        configuration["AOC_SESSION_COOKIE"]

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

    let GetInputData day = getPuzzleInput day |> Async.RunSynchronously