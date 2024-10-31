using System.Collections.Concurrent;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;

namespace _2016;

public abstract partial class _14
{
    private static readonly string Data;

    static _14() => Data = Task.Run(() => Utils.GetInputData(14)).Result.Trim();

    private static string GetHash(int index, bool stretched = false)
    {
        var input = Data + index;
        var hashBytes = MD5.HashData(Encoding.ASCII.GetBytes(input));
        var hash = ConvertHashToString(hashBytes);

        if (!stretched) return hash;
        for (var i = 0; i < 2016; i++)
        {
            hashBytes = MD5.HashData(Encoding.ASCII.GetBytes(hash));
            hash = ConvertHashToString(hashBytes);
        }

        return hash;
    }

    private static string ConvertHashToString(byte[] hashBytes)
    {
        var sb = new StringBuilder();
        foreach (var b in hashBytes) sb.Append(b.ToString("x2"));

        return sb.ToString();
    }

    private static bool CheckForQuintuple(ConcurrentDictionary<int, string> hashes, int startIndex, char character)
    {
        for (var i = startIndex + 1; i <= startIndex + 1000; i++)
            if (hashes[i].Contains(new string(character, 5))) return true;

        return false;
    }

    private static int SolvePartOne()
    {
        var index = 0;
        var keysFound = 0;
        var potentialKeys = new ConcurrentBag<(int Index, char Character)>();
        var hashes = new ConcurrentDictionary<int, string>();

        const int precomputeLimit = 30000;
        Parallel.For(0, precomputeLimit, i => hashes[i] = GetHash(i));

        while (keysFound < 64 && index < precomputeLimit - 1000)
        {
            var tripletMatch = IsTriplet().Match(hashes[index]);
            if (tripletMatch.Success) potentialKeys.Add((index, tripletMatch.Groups[1].Value[0]));

            Parallel.ForEach(potentialKeys.ToArray(), key =>
            {
                if (!CheckForQuintuple(hashes, key.Index, key.Character)) return;
                keysFound++;
                potentialKeys = new ConcurrentBag<(int Index, char Character)>(potentialKeys.Where(k => k.Index != key.Index));
            });

            if (keysFound == 64) return index;

            index++;
        }

        return -1;
    }

    private static int SolvePartTwo()
    {
        var index = 0;
        var keysFound = 0;
        var potentialKeys = new ConcurrentBag<(int Index, char Character)>();
        var hashes = new ConcurrentDictionary<int, string>();

        const int precomputeLimit = 30000;
        Parallel.For(0, precomputeLimit, i => hashes[i] = GetHash(i, true));

        while (keysFound < 64 && index < precomputeLimit - 1000)
        {
            var tripletMatch = IsTriplet().Match(hashes[index]);
            if (tripletMatch.Success) potentialKeys.Add((index, tripletMatch.Groups[1].Value[0]));

            Parallel.ForEach(potentialKeys.ToArray(), key =>
            {
                if (!CheckForQuintuple(hashes, key.Index, key.Character)) return;
                keysFound++;
                potentialKeys = new ConcurrentBag<(int Index, char Character)>(potentialKeys.Where(k => k.Index != key.Index));
            });

            if (keysFound == 64) return index;

            index++;
        }

        return -1;
    }

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    [GeneratedRegex(@"(\w)\1\1")]
    private static partial Regex IsTriplet();
}