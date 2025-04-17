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

local function countOrbits(object)
    local count = 0
    while data[object] do
        object = data[object]
        count = count + 1
    end
    return count
end

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

local function solvePartOne()
    local totalOrbits = 0
    for object, _ in pairs(data) do
        totalOrbits = totalOrbits + countOrbits(object)
    end
    return totalOrbits
end

local function solvePartTwo()
    local youOrbit = data["YOU"]
    local sanOrbit = data["SAN"]
    return bfs(youOrbit, sanOrbit)
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
