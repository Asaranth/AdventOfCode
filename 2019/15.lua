local utils = require("utils")

local data = {}
for value in utils.getInputData(15):gmatch("[^,]+") do
    table.insert(data, tonumber(value))
end

local NORTH, SOUTH, WEST, EAST = 1, 2, 3, 4
local HIT_WALL, MOVED, FOUND_OXYGEN = 0, 1, 2
local DIRECTIONS = {
    [NORTH] = { x = 0, y = 1 },
    [SOUTH] = { x = 0, y = -1 },
    [WEST] = { x = -1, y = 0 },
    [EAST] = { x = 1, y = 0 }
}

local function cloneComputer(original)
    local new = utils.intcode({})
    for k, v in pairs(original.memory) do
        new.memory[k] = v
    end
    new.ip = original.ip
    new.relativeBase = original.relativeBase
    new.inputs = { table.unpack(original.inputs) }
    new.outputs = { table.unpack(original.outputs) }
    new.halted = original.halted
    return new
end

local function buildCompleteMap()
    local computer = utils.intcode(data)
    local position = { x = 0, y = 0 }
    local map = { ["0,0"] = "." }
    local queue = { { pos = position, computer = computer } }
    local oxygenPos = nil
    while #queue > 0 do
        local current = table.remove(queue, 1)
        local pos = current.pos
        for dir = 1, 4 do
            local newPos = { x = pos.x + DIRECTIONS[dir].x, y = pos.y + DIRECTIONS[dir].y }
            local posKey = string.format("%d,%d", newPos.x, newPos.y)
            if not map[posKey] then
                local newComputer = cloneComputer(current.computer)
                newComputer:addInput(dir)
                newComputer:run()
                local status = newComputer:getOutput()
                if status == FOUND_OXYGEN then
                    map[posKey] = "O"
                    oxygenPos = newPos
                    table.insert(queue, { pos = newPos, computer = newComputer })
                elseif status == MOVED then
                    map[posKey] = "."
                    table.insert(queue, { pos = newPos, computer = newComputer })
                elseif status == HIT_WALL then
                    map[posKey] = "#"
                end
            end
        end
    end
    return map, oxygenPos
end

local function solvePartOne()
    local computer = utils.intcode(data)
    local position = { x = 0, y = 0 }
    local visited = { ["0,0"] = true }
    local queue = { { pos = position, steps = 0, computer = computer } }
    while #queue > 0 do
        local current = table.remove(queue, 1)
        local pos = current.pos
        for dir = 1, 4 do
            local newPos = { x = pos.x + DIRECTIONS[dir].x, y = pos.y + DIRECTIONS[dir].y }
            local posKey = string.format("%d,%d", newPos.x, newPos.y)
            if not visited[posKey] then
                local newComputer = cloneComputer(current.computer)
                newComputer:addInput(dir)
                newComputer:run()
                local status = newComputer:getOutput()
                if status == FOUND_OXYGEN then
                    return current.steps + 1
                elseif status == MOVED then
                    visited[posKey] = true
                    table.insert(queue, { pos = newPos, steps = current.steps + 1, computer = newComputer })
                elseif status == HIT_WALL then
                    visited[posKey] = true
                end
            end
        end
    end
    return "No path found"
end

local function solvePartTwo()
    local map, oxygenPos = buildCompleteMap()
    if not oxygenPos then return "Oxygen system not found" end
    local minutes = 0
    while true do
        local newOxygen = {}
        local filledSomething = false
        for posKey, tile in pairs(map) do
            if tile == "O" then
                local x, y = posKey:match("(-?%d+),(-?%d+)")
                x, y = tonumber(x), tonumber(y)
                for _, dir in ipairs(DIRECTIONS) do
                    local newPos = { x = x + dir.x, y = y + dir.y }
                    local newKey = string.format("%d,%d", newPos.x, newPos.y)
                    if map[newKey] == "." then
                        newOxygen[newKey] = true
                        filledSomething = true
                    end
                end
            end
        end
        if not filledSomething then break end
        for posKey in pairs(newOxygen) do
            map[posKey] = "O"
        end
        minutes = minutes + 1
    end
    return minutes
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
