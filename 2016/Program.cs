namespace _2016;

public abstract class Program
{
    private static readonly Dictionary<int, Action> Solutions = new()
    {
        { 1, _01.Run },
        { 2, _02.Run },
        { 3, _03.Run },
        { 4, _04.Run },
        { 5, _05.Run },
        { 6, _06.Run },
        { 7, _07.Run },
        { 8, _08.Run },
        { 9, _09.Run },
        { 10, _10.Run },
        { 11, _11.Run },
        { 12, _12.Run },
        { 13, _13.Run },
        { 14, _14.Run },
        { 15, _15.Run },
        { 16, _16.Run },
        { 17, _17.Run },
        { 18, _18.Run },
        { 19, _19.Run },
        { 20, _20.Run },
        { 21, _21.Run },
        { 22, _22.Run },
        { 23, _23.Run },
        { 24, _24.Run },
        { 25, _25.Run }
    };

    private static void Main()
    {
        Console.Write("Enter the day number you want to run (1-25): ");
        if (int.TryParse(Console.ReadLine(), out var dayNumber) && dayNumber is >= 1 and <= 25) RunSolution(dayNumber);
        else Console.WriteLine("Invalid input. Please enter a number between 1 and 25.");
    }

    private static void RunSolution(int dayNumber)
    {
        if (Solutions.TryGetValue(dayNumber, out var runAction)) runAction();
        else Console.WriteLine("Solution for the given day is not implemented yet.");
    }
}