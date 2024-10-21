using Microsoft.Extensions.Configuration;

namespace _2016;

public static class Utils
{
    private static readonly IConfigurationRoot Configuration;

    static Utils()
    {
        var basePath = AppContext.BaseDirectory;
        Configuration = new ConfigurationBuilder()
            .SetBasePath(basePath)
            .AddJsonFile(Path.Combine(basePath, "../../../appsettings.json"))
            .Build();
    }

    public static async Task<string> GetInputData(int day)
    {
        var cacheFile = Path.Combine("data", $"{day:00}.txt");
        if (File.Exists(cacheFile)) return await File.ReadAllTextAsync(cacheFile);

        var sessionCookie = Configuration["AOC_SESSION_COOKIE"];

        if (string.IsNullOrEmpty(sessionCookie))
            throw new InvalidOperationException("AdventOfCode session cookie is not configured.");

        var url = $"https://adventofcode.com/2016/day/{day}/input";

        using var httpClient = new HttpClient();
        httpClient.DefaultRequestHeaders.Add("Cookie", $"session={sessionCookie}");
        var response = await httpClient.GetAsync(url);

        if (!response.IsSuccessStatusCode)
            throw new HttpRequestException($"Failed to fetch data: {response.ReasonPhrase}");

        var data = await response.Content.ReadAsStringAsync();

        Directory.CreateDirectory("data");
        await File.WriteAllTextAsync(cacheFile, data);

        return data;
    }
}