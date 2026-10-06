--- Day 06: Universal Orbit Map
---
--- Traverses directed and undirected orbital trees to calculate total orbits and shortest path transfers.

local utils = require("utils")

local orbits = {}
local data = {}
for line in utils.getInputData(6):gmatch("[^\r\n]+") do
    local parent, child = line:match("(%w+)%)?(%w+)")
    if parent and child then
        orbits[parent] = orbits[parent] or {}
        orbits[child] = orbits[child] or {}
        table.insert(orbits[parent], child)
        table.insert(orbits[child], parent)
        data[child] = parent
    end
end

--- Counts direct and indirect orbits for a given celestial body by traversing parent links to the Centre of Mass (COM).
---
--- @param object string The name of the celestial body.
--- @return number The total number of direct and indirect orbits.
local function countOrbits(object)
    local count = 0
    while data[object] do
        object = data[object]
        count = count + 1
    end
    return count
end

--- Performs a breadth-first search to find the minimum orbital transfers between two bodies.
---
--- @param start string The starting body identifier.
--- @param target string The destination body identifier.
--- @return number|nil The minimum number of orbital transfers, or nil if no path exists.
local function bfs(start, target)
    local queue = { { start, 0 } }
    local visited = {}
    while #queue > 0 do
        local current, distance = table.unpack(table.remove(queue, 1))
        if current == target then
            return distance
        end
        visited[current] = true
        for _, neighbor in ipairs(orbits[current] or {}) do
            if not visited[neighbor] then
                table.insert(queue, { neighbor, distance + 1 })
            end
        end
    end
    return nil
end

--- Calculates the total number of direct and indirect orbits in the orbit map for Part One.
---
--- @return number The sum of all direct and indirect orbits.
local function solvePartOne()
    local totalOrbits = 0
    for object, _ in pairs(data) do
        totalOrbits = totalOrbits + countOrbits(object)
    end
    return totalOrbits
end

--- Calculates the minimum number of orbital transfers required to travel from YOU to SAN for Part Two.
---
--- @return number|nil The minimum transfer distance.
local function solvePartTwo()
    local youOrbit = data["YOU"]
    local sanOrbit = data["SAN"]
    return bfs(youOrbit, sanOrbit)
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
