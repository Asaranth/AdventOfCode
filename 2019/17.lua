--- Day 17: Set and Forget
---
--- Parses ASCII scaffold maps to identify intersections and commands the vacuum robot via movement routines.

local utils = require("utils")

local data = {}
for value in utils.getInputData(17):gmatch("[^,]+") do
    table.insert(data, tonumber(value))
end

--- Converts ASCII output codes from the Intcode computer into a 2D character grid.
---
--- @param output number[] List of ASCII byte values emitted by the computer.
--- @return string[][] 2D array of character cells representing the scaffold view.
local function parseOutput(output)
    local grid = {}
    local row = {}
    for _, value in ipairs(output) do
        if value == 10 then
            if #row > 0 then
                table.insert(grid, row)
                row = {}
            end
        else
            table.insert(row, string.char(value))
        end
    end
    return grid
end

--- Identifies scaffold intersection points and calculates the sum of their alignment parameters.
---
--- @param grid string[][] 2D scaffold character grid.
--- @return number The sum of alignment parameters (row * column for all intersections).
local function calculateAlignmentParameters(grid)
    local sum = 0
    for r = 2, #grid - 1 do
        for c = 2, #grid[r] - 1 do
            if grid[r][c] == "#" and
                grid[r - 1][c] == "#" and
                grid[r + 1][c] == "#" and
                grid[r][c - 1] == "#" and
                grid[r][c + 1] == "#" then
                sum = sum + (r - 1) * (c - 1)
            end
        end
    end
    return sum
end

--- Solves Part One by running the camera program and summing intersection alignment parameters.
---
--- @return number The total sum of alignment parameters.
local function solvePartOne()
    local computer = utils.intcode(data)
    computer:run()
    local output = computer.outputs
    local grid = parseOutput(output)
    return calculateAlignmentParameters(grid)
end

--- Solves Part Two by commanding the vacuum robot through decomposed movement functions.
---
--- @return number Total dust collected by the vacuum robot.
local function solvePartTwo()
    local dataCopy = { table.unpack(data) }
    dataCopy[1] = 2

    -- Constants from observation
    local FUNC_A = "R,10,R,8,L,10,L,10"
    local FUNC_B = "R,8,L,6,L,6"
    local FUNC_C = "L,10,R,10,L,6"
    local MAIN_ROUTINE = "A,B,B,A,C,B,C,C,B,A"

    local movementInputs = {}

    local function addAscii(input)
        for i = 1, #input do
            table.insert(movementInputs, input:byte(i))
        end
        table.insert(movementInputs, 10)
    end

    addAscii(MAIN_ROUTINE)
    addAscii(FUNC_A)
    addAscii(FUNC_B)
    addAscii(FUNC_C)
    addAscii("n")

    local computer = utils.intcode(dataCopy)
    for _, input in ipairs(movementInputs) do
        computer:addInput(input)
    end
    computer:run()
    local dustCollected = 0
    for _, value in ipairs(computer.outputs) do
        if value > 127 then
            dustCollected = value
        end
    end
    return dustCollected
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
