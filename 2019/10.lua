--- Day 10: Monitoring Station
---
--- Calculates direct line-of-sight visibility and simulates rotating laser vaporisation of asteroids.

local utils = require("utils")

local data = {}
for line in utils.getInputData(10):gmatch("[^\r\n]+") do
    table.insert(data, line)
end
local asteroids = {}
for y, line in ipairs(data) do
    for x = 1, #line do
        if line:sub(x, x) == "#" then
            table.insert(asteroids, { x = x - 1, y = y - 1 })
        end
    end
end

--- Computes the greatest common divisor of two integers.
---
--- @param a number First integer.
--- @param b number Second integer.
--- @return number The greatest common divisor.
local function gcd(a, b)
    if b == 0 then return math.abs(a) end
    return gcd(b, a % b)
end

--- Groups all visible target asteroids by their radial angle from a given monitoring base.
---
--- @param base { x: number, y: number } The asteroid coordinates serving as the base station.
--- @return table<number, { angle: number, targets: { x: number, y: number }[] }> Map of angle radians to target lists.
local function getAngles(base)
    local angles = {}
    for _, target in ipairs(asteroids) do
        if base.x ~= target.x or base.y ~= target.y then
            local dx = target.x - base.x
            local dy = target.y - base.y
            local angle = math.atan(-dy, dx)
            if angle < 0 then
                angle = angle + 2 * math.pi
            end
            if not angles[angle] then
                angles[angle] = { angle = angle, targets = {} }
            end
            table.insert(angles[angle].targets, { x = target.x, y = target.y })
        end
    end
    return angles
end

--- Finds the optimal asteroid location that can detect the maximum number of distinct asteroids.
---
--- @return number The maximum count of visible asteroids.
--- @return { x: number, y: number } The coordinates of the best monitoring station location.
local function solvePartOne()
    local maxVisible = 0
    local bestLocation = nil
    for _, asteroid in ipairs(asteroids) do
        local angles = getAngles(asteroid)
        local visible = 0
        for _ in pairs(angles) do
            visible = visible + 1
        end
        if visible > maxVisible then
            maxVisible = visible
            bestLocation = asteroid
        end
    end
    return maxVisible, bestLocation
end

--- Simulates the clockwise rotating laser vaporising asteroids and finds the 200th asteroid vaporised.
---
--- @param location { x: number, y: number } The base station asteroid coordinates.
--- @return number The coordinate encoding 100 * x + y of the 200th vaporised asteroid.
local function solvePartTwo(location)
    local angles = {}
    for _, asteroid in ipairs(asteroids) do
        if not (location.x == asteroid.x and location.y == asteroid.y) then
            local dx = asteroid.x - location.x
            local dy = location.y - asteroid.y
            local angle = math.atan(dx, dy) % (2 * math.pi)
            if not angles[angle] then
                angles[angle] = {}
            end
            local distance = math.sqrt(dx * dx + dy * dy)
            table.insert(angles[angle], { x = asteroid.x, y = asteroid.y, distance = distance })
        end
    end
    for _, group in pairs(angles) do
        table.sort(group, function(a, b)
            return a.distance < b.distance
        end)
    end
    local sortedAngles = {}
    for angle in pairs(angles) do
        table.insert(sortedAngles, angle)
    end
    table.sort(sortedAngles)
    local vaporizedCount = 0
    local vaporizedAsteroid = nil
    while vaporizedCount < 200 do
        for _, angle in ipairs(sortedAngles) do
            local group = angles[angle]
            if #group > 0 then
                vaporizedCount = vaporizedCount + 1
                vaporizedAsteroid = table.remove(group, 1)
                if vaporizedCount == 200 then
                    if vaporizedAsteroid then
                        return vaporizedAsteroid.x * 100 + vaporizedAsteroid.y
                    else
                        error("Not enough asteroids to vaporize the 200th.")
                    end
                end
            end
        end
    end
end

local maxVisible, bestLocation = solvePartOne()
print("Part One: " .. maxVisible)
print("Part Two: " .. solvePartTwo(bestLocation))
