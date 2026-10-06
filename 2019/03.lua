--- Day 03: Crossed Wires
---
--- Traces wire paths on a 2D grid to compute Manhattan distance and signal delay intersections.

local utils = require("utils")

--- Parses the puzzle input into a list of wire path strings.
---
--- @return string[] The movement paths for each wire.
local function parseInput()
    local data = {}
    for line in utils.getInputData(3):gmatch("[^\r\n]+") do
        table.insert(data, line)
    end
    return data
end

--- Traces a wire's path across 2D coordinates, recording visited points or step counts.
---
--- @param path string Comma-separated movement instructions (e.g. "R75,D30").
--- @param withSteps boolean When true, records the earliest step count to each point; otherwise records presence.
--- @return table<string, number|boolean> Map of coordinate keys ("x,y") to step count or true.
local function traceWirePath(path, withSteps)
    local x, y = 0, 0
    local visited = {}
    local steps = 0
    for step in path:gmatch("([^,]+)") do
        local dir = step:sub(1, 1)
        local dist = tonumber(step:sub(2))
        for _ = 1, dist do
            steps = steps + 1
            if dir == "R" then
                x = x + 1
            elseif dir == "L" then
                x = x - 1
            elseif dir == "U" then
                y = y + 1
            elseif dir == "D" then
                y = y - 1
            end
            local pointKey = x .. "," .. y
            if withSteps then
                if not visited[pointKey] then
                    visited[pointKey] = steps
                end
            else
                visited[pointKey] = true
            end
        end
    end
    return visited
end

--- Calculates the minimum Manhattan distance from the origin to any wire intersection point.
---
--- @return number The minimum Manhattan distance.
local function solvePartOne()
    local wires = parseInput()
    local wire1Path = traceWirePath(wires[1], false)
    local wire2Path = traceWirePath(wires[2], false)
    local closestDistance = math.huge
    for point in pairs(wire1Path) do
        if wire2Path[point] then
            local x, y = point:match("([^,]+),([^,]+)")
            local distance = math.abs(tonumber(x) or 0) + math.abs(tonumber(y) or 0)
            if distance < closestDistance then
                closestDistance = distance
            end
        end
    end
    return closestDistance
end

--- Calculates the fewest combined steps required for both wires to reach an intersection point.
---
--- @return number The minimum total step count across all intersections.
local function solvePartTwo()
    local wires = parseInput()
    local wire1Path = traceWirePath(wires[1], true)
    local wire2Path = traceWirePath(wires[2], true)
    local fewestSteps = math.huge
    for point, steps1 in pairs(wire1Path) do
        local steps2 = wire2Path[point]
        if steps2 then
            local totalSteps = steps1 + steps2
            if totalSteps < fewestSteps then
                fewestSteps = totalSteps
            end
        end
    end
    return fewestSteps
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
