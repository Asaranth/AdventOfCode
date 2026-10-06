using System.Security.Cryptography;
using System.Text;

namespace _2016;

/// <summary>
/// Day 05: How About a Nice Game of Chess?
/// 
/// Mines MD5 hash prefixes to discover door security passwords sequentially and positional.
/// </summary>
public abstract class _05
{
    private static readonly string Data;

    static _05() => Data = Task.Run(() => Utils.GetInputData(5)).Result.Trim();

    /// <summary>
    /// Computes the MD5 hex digest string for a door ID concatenated with an index integer.
    /// </summary>
    /// <param name="doorId">The door ID string.</param>
    /// <param name="index">The integer nonce/index.</param>
    /// <returns>Uppercase hex string of the MD5 hash.</returns>
    private static string ComputeHash(string doorId, int index)
    {
        var input = doorId + index;
        var hashBytes = MD5.HashData(Encoding.ASCII.GetBytes(input));
        return BitConverter.ToString(hashBytes).Replace("-", "").ToUpper();
    }

    /// <summary>
    /// Solves Part One: constructs an eight-character password from the sixth character of hashes with five leading zeroes.
    /// </summary>
    /// <returns>The eight-character password string.</returns>
    private static string SolvePartOne()
    {
        var password = new StringBuilder();
        var index = 0;

        while (password.Length < 8)
        {
            var hash = ComputeHash(Data, index);

            if (hash.StartsWith("00000")) password.Append(hash[5]);

            index++;
        }
        return password.ToString();
    }

    /// <summary>
    /// Solves Part Two: constructs the password by placing seventh character at positional indices indicated by the sixth character.
    /// </summary>
    /// <returns>The decrypted eight-character password.</returns>
    private static string SolvePartTwo()
    {
        const int passwordLength = 8;
        var password = new char?[passwordLength];
        var filledPositions = 0;
        var index = 0;

        while (filledPositions < passwordLength)
        {
            var hash = ComputeHash(Data, index);

            if (hash.StartsWith("00000"))
            {
                var positionChar = hash[5];
                if (char.IsDigit(positionChar))
                {
                    var position = positionChar - '0';
                    if (position < passwordLength && !password[position].HasValue)
                    {
                        password[position] = hash[6];
                        filledPositions++;
                    }
                }
            }

            index++;
        }

        return new string(password.Select(c => c ?? '_').ToArray());
    }

    /// <summary>
    /// Executes and prints the solutions for Part One and Part Two.
    /// </summary>
    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}