local utils = require("utils")

local data = {}
for line in utils.getInputData(20):gmatch("[^\n]+") do
    table.insert(data, line)
end

local function parseMaze()
    local grid = {}
    local portals = {}
    for y = 1, #data do
        grid[y] = {}
        for x = 1, #data[y] do
            grid[y][x] = data[y]:sub(x, x)
        end
    end
    for y = 1, #grid do
        for x = 1, #grid[y] - 1 do
            local c1 = grid[y][x]
            local c2 = grid[y][x + 1]
            if c1:match("%u") and c2:match("%u") then
                local label = c1 .. c2
                local passageX, passageY
                if x > 1 and grid[y][x - 1] == '.' then
                    passageX, passageY = x - 1, y
                elseif x + 2 <= #grid[y] and grid[y][x + 2] == '.' then
                    passageX, passageY = x + 2, y
                end
                if passageX then
                    if not portals[label] then
                        portals[label] = {}
                    end
                    table.insert(portals[label], { x = passageX, y = passageY })
                end
            end
        end
    end
    for y = 1, #grid - 1 do
        for x = 1, #grid[y] do
            local c1 = grid[y][x]
            local c2 = (grid[y + 1] and grid[y + 1][x]) or ' '
            if c1:match("%u") and c2:match("%u") then
                local label = c1 .. c2
                local passageX, passageY
                if y > 1 and grid[y - 1][x] == '.' then
                    passageX, passageY = x, y - 1
                elseif y + 2 <= #grid and grid[y + 2] and grid[y + 2][x] == '.' then
                    passageX, passageY = x, y + 2
                end
                if passageX then
                    if not portals[label] then
                        portals[label] = {}
                    end
                    table.insert(portals[label], { x = passageX, y = passageY })
                end
            end
        end
    end
    return grid, portals
end

local function isOuterEdge(x, y, grid)
    local minX, maxX = math.huge, 0
    local minY, maxY = math.huge, 0
    for row = 1, #grid do
        for col = 1, #grid[row] do
            if grid[row][col] == '#' or grid[row][col] == '.' then
                minX = math.min(minX, col)
                maxX = math.max(maxX, col)
                minY = math.min(minY, row)
                maxY = math.max(maxY, row)
            end
        end
    end
    return x <= minX + 2 or x >= maxX - 2 or y <= minY + 2 or y >= maxY - 2
end

local function createPortalMap(portals)
    local portalMap = {}
    for label, positions in pairs(portals) do
        if #positions == 2 and label ~= "AA" and label ~= "ZZ" then
            local pos1 = positions[1]
            local pos2 = positions[2]
            local key1 = pos1.x .. "," .. pos1.y
            local key2 = pos2.x .. "," .. pos2.y
            portalMap[key1] = pos2
            portalMap[key2] = pos1
        end
    end
    return portalMap
end

local function createOuterPortalMap(portals, grid)
    local outerMap = {}
    for _, positions in pairs(portals) do
        for _, pos in ipairs(positions) do
            local key = pos.x .. "," .. pos.y
            outerMap[key] = isOuterEdge(pos.x, pos.y, grid)
        end
    end
    return outerMap
end

local function solvePartOne()
    local grid, portals = parseMaze()
    local portalMap = createPortalMap(portals)
    local start = portals["AA"][1]
    local goal = portals["ZZ"][1]
    local queue = { { x = start.x, y = start.y, steps = 0 } }
    local visited = {}
    visited[start.x .. "," .. start.y] = true
    local directions = { { 0, 1 }, { 1, 0 }, { 0, -1 }, { -1, 0 } }
    while #queue > 0 do
        local current = table.remove(queue, 1)
        if current.x == goal.x and current.y == goal.y then
            return current.steps
        end
        for _, dir in ipairs(directions) do
            local nx = current.x + dir[1]
            local ny = current.y + dir[2]
            local key = nx .. "," .. ny
            if not visited[key] and grid[ny] and grid[ny][nx] == '.' then
                visited[key] = true
                table.insert(queue, { x = nx, y = ny, steps = current.steps + 1 })
            end
        end
        local currentKey = current.x .. "," .. current.y
        if portalMap[currentKey] then
            local portal = portalMap[currentKey]
            local portalKey = portal.x .. "," .. portal.y
            if not visited[portalKey] then
                visited[portalKey] = true
                table.insert(queue, { x = portal.x, y = portal.y, steps = current.steps + 1 })
            end
        end
    end
    return 0
end

local function solvePartTwo()
    local grid, portals = parseMaze()
    local portalMap = createPortalMap(portals)
    local outerMap = createOuterPortalMap(portals, grid)
    local start = portals["AA"][1]
    local goal = portals["ZZ"][1]
    local queue = { { x = start.x, y = start.y, level = 0, steps = 0 } }
    local visited = {}
    visited[start.x .. "," .. start.y .. "," .. 0] = true
    local directions = { { 0, 1 }, { 1, 0 }, { 0, -1 }, { -1, 0 } }
    while #queue > 0 do
        local current = table.remove(queue, 1)
        if current.x == goal.x and current.y == goal.y and current.level == 0 then
            return current.steps
        end
        for _, dir in ipairs(directions) do
            local nx = current.x + dir[1]
            local ny = current.y + dir[2]
            local key = nx .. "," .. ny .. "," .. current.level
            if not visited[key] and grid[ny] and grid[ny][nx] == '.' then
                visited[key] = true
                table.insert(queue, { x = nx, y = ny, level = current.level, steps = current.steps + 1 })
            end
        end
        local currentKey = current.x .. "," .. current.y
        if portalMap[currentKey] then
            local isOuter = outerMap[currentKey]
            if not (current.level == 0 and isOuter) then
                local portal = portalMap[currentKey]
                local newLevel = current.level
                if isOuter then
                    newLevel = current.level - 1
                else
                    newLevel = current.level + 1
                end
                local portalKey = portal.x .. "," .. portal.y .. "," .. newLevel
                if not visited[portalKey] then
                    visited[portalKey] = true
                    table.insert(queue, { x = portal.x, y = portal.y, level = newLevel, steps = current.steps + 1 })
                end
            end
        end
    end
    return 0
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())