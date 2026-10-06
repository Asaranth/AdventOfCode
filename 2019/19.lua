--- Day 19: Tractor Beam
---
--- Probes tractor beam emission coordinates using the drone controller Intcode program.

local utils = require("utils")

--- Parses the puzzle input into an array of integer Intcode instructions.
---
--- @return number[] The parsed Intcode program instructions.
local function parseInput()
    local data = {}
    for value in utils.getInputData(19):gmatch("[^,]+") do
        table.insert(data, tonumber(value))
    end
    return data
end

--- Queries the tractor beam drone program at a given (x, y) coordinate.
---
--- @param program number[] The tractor beam Intcode program.
--- @param x number Horizontal coordinate.
--- @param y number Vertical coordinate.
--- @return number 1 if the drone is pulled by the beam, 0 otherwise.
local function checkPosition(program, x, y)
    local computer = utils.intcode(program)
    computer:addInput(x)
    computer:addInput(y)
    computer:run()
    return computer:getOutput()
end

--- Counts the total points affected by the tractor beam within a 50x50 area for Part One.
---
--- @return number The number of beam-affected coordinate points.
local function solvePartOne()
    local program = parseInput()
    local count = 0
    for y = 0, 49 do
        for x = 0, 49 do
            local result = checkPosition(program, x, y)
            if result == 1 then
                count = count + 1
            end
        end
    end
    return count
end

--- Finds the top-left coordinate of the closest 100x100 square fitting entirely within the beam for Part Two.
---
--- @return number The coordinate encoded as x * 10000 + y.
local function solvePartTwo()
    local program = parseInput()
    local size = 100
    local y = size
    local x = 0
    while checkPosition(program, x, y) == 0 do
        x = x + 1
    end
    while true do
        local topLeftY = y - size + 1
        if topLeftY >= 0 and checkPosition(program, x, topLeftY) == 1 then
            if checkPosition(program, x + size - 1, topLeftY) == 1 then
                return x * 10000 + topLeftY
            end
        end
        y = y + 1
        while checkPosition(program, x, y) == 0 do
            x = x + 1
        end
    end
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())