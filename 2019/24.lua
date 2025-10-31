local utils = require("utils")

local data = {}
for line in utils.getInputData(24):gmatch("[^\n]+") do
    table.insert(data, line)
end

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