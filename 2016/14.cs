using System.Collections.Concurrent;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;

namespace _2016;

/// <summary>
/// Day 14: One-Time Pad
/// 
/// Generates one-time pad keys via sequential MD5 hashing and 2016-iteration key stretching.
/// </summary>
public abstract partial class _14
{
    private static readonly string Data;

    static _14() => Data = Task.Run(() => Utils.GetInputData(14)).Result.Trim();

    /// <summary>
    /// Computes the MD5 hex digest for the salt concatenated with index, optionally applying 2016 rounds of key stretching.
    /// </summary>
    /// <param name="index">Integer nonce index.</param>
    /// <param name="stretched">If true, rehashes the hex string 2016 times.</param>
    /// <returns>Lowercase hex string of the MD5 hash.</returns>
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

    /// <summary>
    /// Converts a byte array into a lowercase hexadecimal string.
    /// </summary>
    /// <param name="hashBytes">Raw hash bytes.</param>
    /// <returns>Hexadecimal string representation.</returns>
    private static string ConvertHashToString(byte[] hashBytes)
    {
        var sb = new StringBuilder();
        foreach (var b in hashBytes) sb.Append(b.ToString("x2"));

        return sb.ToString();
    }

    /// <summary>
    /// Checks whether any of the next 1000 consecutive hashes contains five repetitions of the specified character.
    /// </summary>
    /// <param name="hashes">Concurrent map of precomputed or generated hash values.</param>
    /// <param name="startIndex">The index of the candidate key.</param>
    /// <param name="character">The character to check five of in sequence.</param>
    /// <returns>True if a quintuple match is found within 1000 hashes; otherwise, false.</returns>
    private static bool CheckForQuintuple(ConcurrentDictionary<int, string> hashes, int startIndex, char character)
    {
        for (var i = startIndex + 1; i <= startIndex + 1000; i++)
            if (hashes[i].Contains(new string(character, 5))) return true;

        return false;
    }

    /// <summary>
    /// Solves Part One: finds the index producing the 64th one-time pad key using standard MD5 hashing.
    /// </summary>
    /// <returns>The index of the 64th key.</returns>
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

    /// <summary>
    /// Solves Part Two: finds the index producing the 64th one-time pad key using 2016-iteration key stretching.
    /// </summary>
    /// <returns>The index of the 64th stretched key.</returns>
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

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }

    /// <summary>
    /// Regex pattern matching three consecutive identical alphanumeric characters.
    /// </summary>
    [GeneratedRegex(@"(\w)\1\1")]
    private static partial Regex IsTriplet();
}