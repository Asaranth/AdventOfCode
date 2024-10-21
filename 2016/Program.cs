namespace _2016;

public abstract class Program
{
    private static void Main(string[] args)
    {
        Console.Write("Enter the day number you want to run (1-25): ");
        if (int.TryParse(Console.ReadLine(), out var dayNumber) && dayNumber is >= 1 and <= 25) RunSolution(dayNumber);
        else Console.WriteLine("Invalid input. Please enter a number between 1 and 25.");
    }

    private static void RunSolution(int dayNumber)
    {
        switch (dayNumber)
        {
            case 1:
                _01.Run();
                break;
            case 2:
                _02.Run();
                break;
            case 3:
                _03.Run();
                break;
            case 4:
                _04.Run();
                break;
            case 5:
                _05.Run();
                break;
            case 6:
                _06.Run();
                break;
            case 7:
                _07.Run();
                break;
            default:
                Console.WriteLine("Solution for the given day is not implemented yet.");
                break;
        }
    }
}