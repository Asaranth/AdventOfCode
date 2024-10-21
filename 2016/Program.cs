namespace _2016;

internal abstract class Program
{
    private static void Main(string[] args)
    {
        Console.Write("Enter the day number you want to run (1-25): ");
        if (int.TryParse(Console.ReadLine(), out var day) && day is >= 1 and <= 25) RunSolution(day);
        else Console.WriteLine("Invalid input. Please enter a number between 1 and 25.");
    }

    private static void RunSolution(int day)
    {
        switch (day)
        {
            case 1:
                _01.Run();
                break;
            case 2:
                _02.Run();
                break;
            default:
                Console.WriteLine("Solution for the given day is not implemented yet.");
                break;
        }
    }
}