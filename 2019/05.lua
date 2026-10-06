--- Day 05: Sunny with a Chance of Asteroids
---
--- Runs diagnostic tests on the Intcode computer using parameter modes, jumps, and comparisons.

local utils = require("utils")

--- Parses the puzzle input into a list of integer Intcode instructions.
---
--- @return number[] The parsed Intcode program instructions.
local function parseInput()
    local data = {}
    for value in utils.getInputData(5):gmatch("[^,]+") do
        table.insert(data, tonumber(value))
    end
    return data
end

--- Runs the diagnostic program with a given system ID and returns the final non-zero diagnostic code.
---
--- @param input number The system ID input to provide to the diagnostic program (1 for Part One, 5 for Part Two).
--- @return number The final diagnostic code output by the computer.
local function solve(input)
    local program = parseInput()
    local computer = utils.intcode(program)
    computer:addInput(input)
    computer:run()
    local output
    while true do
        local value = computer:getOutput()
        if not value then break end
        output = value
    end
    return output
end

print("Part One: " .. solve(1))
print("Part Two: " .. solve(5))
