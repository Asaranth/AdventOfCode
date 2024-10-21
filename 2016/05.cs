using System.Security.Cryptography;
using System.Text;

namespace _2016;

public abstract class _05
{
    private static readonly string Data;

    static _05() => Data = Task.Run(() => Utils.GetInputData(5)).Result.Trim();

	private static string ComputeHash(string doorId, int index)
    {
        var input = doorId + index;
        var hashBytes = MD5.HashData(Encoding.ASCII.GetBytes(input));
        return BitConverter.ToString(hashBytes).Replace("-", "").ToUpper();
    }

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

    public static void Run()
    {
        Console.WriteLine($"Part One: {SolvePartOne()}");
        Console.WriteLine($"Part Two: {SolvePartTwo()}");
    }
}