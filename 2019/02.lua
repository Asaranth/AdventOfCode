--- Day 02: 1202 Program Alarm
---
--- Executes and analyses Intcode programs to find initial state memory parameters.

local utils = require("utils")

--- Parses the puzzle input into an array of integer memory instructions.
---
--- @return number[] The parsed Intcode program instructions.
local function parseInput()
    local data = {}
    for value in utils.getInputData(2):gmatch("[^,]+") do
        table.insert(data, tonumber(value))
    end
    return data
end

--- Solves Part One by restoring the "1202 program alarm" state and running the program.
---
--- @return number The value left at address 0 after execution halts.
local function solvePartOne()
    local program = parseInput()
    program[2] = 12
    program[3] = 2
    local computer = utils.intcode(program)
    computer:run()
    return computer.memory[0]
end

--- Solves Part Two by searching for noun and verb pairs producing the specified target output.
---
--- @param targetOutput number The desired output value at position 0.
--- @return number The formula result 100 * noun + verb.
local function solvePartTwo(targetOutput)
    local originalProgram = parseInput()
    for noun = 0, 99 do
        for verb = 0, 99 do
            local program = { table.unpack(originalProgram) }
            program[2] = noun
            program[3] = verb
            local computer = utils.intcode(program)
            computer:run()
            if computer.memory[0] == targetOutput then
                return 100 * noun + verb
            end
        end
    end
    error("No solution found")
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo(19690720))
