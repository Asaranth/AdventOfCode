using System;
using System.IO;
using System.Collections.Generic;

string[] data = File.ReadAllText("data/01.txt").Split(", ");

(char turn, int distance) ParseInstruction(string instruction) => (instruction[0], int.Parse(instruction.Substring(1)));

char UpdateDirection(char currentDirection, char turn) => currentDirection switch
{
    'N' => turn == 'R' ? 'E' : 'W',
    'E' => turn == 'R' ? 'S' : 'N',
    'S' => turn == 'R' ? 'W' : 'E',
    'W' => turn == 'R' ? 'N' : 'S',
    _ => currentDirection
};

(int X, int Y) Move(int x, int y, char direction, int distance) => direction switch
{
    'N' => (x, y + distance),
    'E' => (x + distance, y),
    'S' => (x, y - distance),
    'W' => (x - distance, y),
    _ => (x, y)
};

int SolvePartOne(string[] instructions)
{
    int x = 0, y = 0;
    char direction = 'N';

    foreach (var instruction in instructions)
    {
        var (turn, distance) = ParseInstruction(instruction);
        direction = UpdateDirection(direction, turn);
        (x, y) = Move(x, y, direction, distance);
    }

    return Math.Abs(x) + Math.Abs(y);
}

int SolvePartTwo(string[] instructions)
{
    int x = 0, y = 0;
    char direction = 'N';
    var visitedLocations = new HashSet<(int X, int Y)>();
    visitedLocations.Add((x, y));

    foreach (var instruction in instructions)
    {
        var (turn, distance) = ParseInstruction(instruction);
        direction = UpdateDirection(direction, turn);

        for (int i = 0; i < distance; i++)
        {
            (x, y) = Move(x, y, direction, 1);
            if (visitedLocations.Contains((x, y))) return Math.Abs(x) + Math.Abs(y);
            visitedLocations.Add((x, y));
        }
    }

    throw new InvalidOperationException("Failed to find a repeated location.");
}

Console.WriteLine($"Part One: {SolvePartOne(data)}");
Console.WriteLine($"Part Two: {SolvePartTwo(data)}");