--- Day 09: Sensor Boost
---
--- Executes the complete Intcode instruction set supporting relative base addressing mode and large integers.

local utils = require("utils")

local data = {}
for line in utils.getInputData(9):gmatch("[^,]+") do
    table.insert(data, tonumber(line))
end

--- Executes the BOOST program with the provided input values and returns the output code.
---
--- @param input number[] List of inputs to queue into the computer (e.g. {1} for test mode, {2} for sensor boost).
--- @return number|nil The diagnostic code or coordinates output by the program.
local function solve(input)
    local computer = utils.intcode(data)
    for _, val in ipairs(input) do
        computer:addInput(val)
    end
    computer:run()
    return computer:getOutput()
end

print("Part One: " .. solve({ 1 }))
print("Part Two: " .. solve({ 2 }))
