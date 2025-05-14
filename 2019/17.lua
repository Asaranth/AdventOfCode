local utils = require("utils")

local data = {}
for value in utils.getInputData(17):gmatch("[^,]+") do
    table.insert(data, tonumber(value))
end

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

local function solvePartOne()
    local computer = utils.intcode(data)
    computer:run()
    local output = computer.outputs
    local grid = parseOutput(output)
    return calculateAlignmentParameters(grid)
end

local function solvePartTwo()
    return 0
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
