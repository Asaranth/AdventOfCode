--- Day 24: Planet of Discord
---
--- Simulates cellular automaton bug colonies on 2D flat and infinite recursive nested dimensional grids.

local utils = require("utils")

local data = {}
for line in utils.getInputData(24):gmatch("[^\n]+") do
    table.insert(data, line)
end

--- Parses the 5x5 grid map into a 2D boolean array of bug states.
---
--- @return boolean[][] 5x5 boolean grid (true for bug, false for empty).
local function parseGrid()
    local grid = {}
    for y = 1, #data do
        grid[y] = {}
        for x = 1, #data[y] do
            grid[y][x] = data[y]:sub(x, x) == '#'
        end
    end
    return grid
end

--- Counts orthogonal adjacent bug neighbours on a standard 5x5 flat grid.
---
--- @param grid boolean[][] 5x5 boolean grid.
--- @param x number Horizontal coordinate (1-5).
--- @param y number Vertical coordinate (1-5).
--- @return number Count of adjacent bugs (0-4).
local function countAdjacentBugs(grid, x, y)
    local count = 0
    local directions = { { 0, -1 }, { 0, 1 }, { -1, 0 }, { 1, 0 } }
    for _, dir in ipairs(directions) do
        local nx, ny = x + dir[1], y + dir[2]
        if ny >= 1 and ny <= 5 and nx >= 1 and nx <= 5 then
            if grid[ny][nx] then
                count = count + 1
            end
        end
    end
    return count
end

--- Advances the 2D flat grid by one minute according to life and death rules.
---
--- @param grid boolean[][] Current 5x5 boolean grid.
--- @return boolean[][] The newly evolved 5x5 boolean grid.
local function evolveGrid(grid)
    local newGrid = {}
    for y = 1, 5 do
        newGrid[y] = {}
        for x = 1, 5 do
            local adjacentBugs = countAdjacentBugs(grid, x, y)
            if grid[y][x] then
                newGrid[y][x] = (adjacentBugs == 1)
            else
                newGrid[y][x] = (adjacentBugs == 1 or adjacentBugs == 2)
            end
        end
    end
    return newGrid
end

--- Calculates the biodiversity rating of a 5x5 grid (sum of 2^i powers for each bug tile).
---
--- @param grid boolean[][] 5x5 boolean grid.
--- @return number The computed biodiversity rating integer.
local function calculateBiodiversity(grid)
    local rating = 0
    local power = 0
    for y = 1, 5 do
        for x = 1, 5 do
            if grid[y][x] then
                rating = rating + (2 ^ power)
            end
            power = power + 1
        end
    end
    return rating
end

--- Counts adjacent bugs for a tile across recursive nested dimensional levels.
---
--- @param levels table<number, boolean[][]> Map of recursive depth levels to 5x5 boolean grids.
--- @param level number Current recursive depth level.
--- @param x number Horizontal coordinate (1-5).
--- @param y number Vertical coordinate (1-5).
--- @return number Count of adjacent bugs across current, inner (+1), and outer (-1) levels.
local function countAdjacentBugsRecursive(levels, level, x, y)
    local count = 0
    local directions = { { x = 0, y = -1 }, { x = 0, y = 1 }, { x = -1, y = 0 }, { x = 1, y = 0 } }
    for _, dir in ipairs(directions) do
        local nx, ny = x + dir.x, y + dir.y
        if nx == 3 and ny == 3 then
            if levels[level + 1] then
                if dir.y == -1 then
                    for i = 1, 5 do
                        if levels[level + 1][5][i] then
                            count = count + 1
                        end
                    end
                elseif dir.y == 1 then
                    for i = 1, 5 do
                        if levels[level + 1][1][i] then
                            count = count + 1
                        end
                    end
                elseif dir.x == -1 then
                    for i = 1, 5 do
                        if levels[level + 1][i][5] then
                            count = count + 1
                        end
                    end
                elseif dir.x == 1 then
                    for i = 1, 5 do
                        if levels[level + 1][i][1] then
                            count = count + 1
                        end
                    end
                end
            end
        elseif nx < 1 or nx > 5 or ny < 1 or ny > 5 then
            if levels[level - 1] then
                if ny < 1 then
                    if levels[level - 1][2][3] then
                        count = count + 1
                    end
                elseif ny > 5 then
                    if levels[level - 1][4][3] then
                        count = count + 1
                    end
                elseif nx < 1 then
                    if levels[level - 1][3][2] then
                        count = count + 1
                    end
                elseif nx > 5 then
                    if levels[level - 1][3][4] then
                        count = count + 1
                    end
                end
            end
        else
            if levels[level][ny][nx] then
                count = count + 1
            end
        end
    end
    return count
end

--- Simulates recursive dimensional grid evolution for a given number of minutes.
---
--- @param levels table<number, boolean[][]> Initial recursive levels map.
--- @param minutes number Number of simulation steps to execute.
--- @return table<number, boolean[][]> The evolved levels map.
local function evolveRecursive(levels, minutes)
    for _ = 1, minutes do
        local newLevels = {}
        local minLevel = math.huge
        local maxLevel = -math.huge
        for level, _ in pairs(levels) do
            minLevel = math.min(minLevel, level)
            maxLevel = math.max(maxLevel, level)
        end
        minLevel = minLevel - 1
        maxLevel = maxLevel + 1
        for level = minLevel, maxLevel do
            if not levels[level] then
                levels[level] = {}
                for y = 1, 5 do
                    levels[level][y] = {}
                    for x = 1, 5 do
                        levels[level][y][x] = false
                    end
                end
            end
            newLevels[level] = {}
            for y = 1, 5 do
                newLevels[level][y] = {}
                for x = 1, 5 do
                    if x == 3 and y == 3 then
                        newLevels[level][y][x] = false
                    else
                        local adjacentBugs = countAdjacentBugsRecursive(levels, level, x, y)
                        if levels[level][y][x] then
                            newLevels[level][y][x] = (adjacentBugs == 1)
                        else
                            newLevels[level][y][x] = (adjacentBugs == 1 or adjacentBugs == 2)
                        end
                    end
                end
            end
        end
        levels = newLevels
    end
    return levels
end

--- Sums all live bugs across all recursive levels.
---
--- @param levels table<number, boolean[][]> Map of recursive depth levels to grids.
--- @return number Total bug count.
local function countTotalBugs(levels)
    local total = 0
    for _, level in pairs(levels) do
        for y = 1, 5 do
            for x = 1, 5 do
                if level[y][x] then
                    total = total + 1
                end
            end
        end
    end
    return total
end

--- Solves Part One by detecting the first repeated grid layout and returning its biodiversity rating.
---
--- @return number The biodiversity rating of the first repeated grid state.
local function solvePartOne()
    local grid = parseGrid()
    local seen = {}
    while true do
        local biodiversity = calculateBiodiversity(grid)
        if seen[biodiversity] then
            return math.floor(biodiversity)
        end
        seen[biodiversity] = true
        grid = evolveGrid(grid)
    end
end

--- Solves Part Two by simulating 200 minutes of recursive dimensional bugs and counting survivors.
---
--- @return number Total bugs alive after 200 minutes.
local function solvePartTwo()
    local initialGrid = parseGrid()
    local levels = {}
    levels[1] = {}
    for y = 1, 5 do
        levels[1][y] = {}
        for x = 1, 5 do
            if x == 3 and y == 3 then
                levels[1][y][x] = false
            else
                levels[1][y][x] = initialGrid[y][x]
            end
        end
    end
    levels = evolveRecursive(levels, 200)
    return countTotalBugs(levels)
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())