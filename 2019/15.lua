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

local Computer = {}
Computer.__index = Computer

function Computer.new(program)
    local self = setmetatable({}, Computer)
    self.memory = {}
    for i, v in ipairs(program) do
        self.memory[i - 1] = v
    end
    self.position = 0
    self.relativeBase = 0
    self.inputs = {}
    self.halted = false
    return self
end

function Computer:clone()
    local new = setmetatable({}, Computer)
    new.memory = {}
    for k, v in pairs(self.memory) do
        new.memory[k] = v
    end
    new.position = self.position
    new.relativeBase = self.relativeBase
    new.inputs = {}
    new.halted = self.halted
    return new
end

function Computer:getParam(position, mode)
    mode = mode or 0
    local value = self.memory[position] or 0
    if mode == 0 then
        return self.memory[value] or 0
    elseif mode == 1 then
        return value
    else
        return self.memory[value + self.relativeBase] or 0
    end
end

function Computer:setParam(position, mode, value)
    mode = mode or 0
    local addr = self.memory[position] or 0
    if mode == 2 then addr = addr + self.relativeBase end
    self.memory[addr] = value
end

function Computer:run(input)
    table.insert(self.inputs, input)
    while true do
        local opcode = self.memory[self.position] or 0
        local instruction = opcode % 100
        local mode1 = math.floor(opcode / 100) % 10
        local mode2 = math.floor(opcode / 1000) % 10
        local mode3 = math.floor(opcode / 10000) % 10
        if instruction == 99 then
            self.halted = true
            return nil
        end
        if instruction == 1 then
            local param1 = self:getParam(self.position + 1, mode1)
            local param2 = self:getParam(self.position + 2, mode2)
            self:setParam(self.position + 3, mode3, param1 + param2)
            self.position = self.position + 4
        elseif instruction == 2 then
            local param1 = self:getParam(self.position + 1, mode1)
            local param2 = self:getParam(self.position + 2, mode2)
            self:setParam(self.position + 3, mode3, param1 * param2)
            self.position = self.position + 4
        elseif instruction == 3 then
            if #self.inputs == 0 then return nil end
            self:setParam(self.position + 1, mode1, table.remove(self.inputs, 1))
            self.position = self.position + 2
        elseif instruction == 4 then
            local output = self:getParam(self.position + 1, mode1)
            self.position = self.position + 2
            return output
        elseif instruction == 5 then
            local param1 = self:getParam(self.position + 1, mode1)
            local param2 = self:getParam(self.position + 2, mode2)
            self.position = param1 ~= 0 and param2 or self.position + 3
        elseif instruction == 6 then
            local param1 = self:getParam(self.position + 1, mode1)
            local param2 = self:getParam(self.position + 2, mode2)
            self.position = param1 == 0 and param2 or self.position + 3
        elseif instruction == 7 then
            local param1 = self:getParam(self.position + 1, mode1)
            local param2 = self:getParam(self.position + 2, mode2)
            self:setParam(self.position + 3, mode3, param1 < param2 and 1 or 0)
            self.position = self.position + 4
        elseif instruction == 8 then
            local param1 = self:getParam(self.position + 1, mode1)
            local param2 = self:getParam(self.position + 2, mode2)
            self:setParam(self.position + 3, mode3, param1 == param2 and 1 or 0)
            self.position = self.position + 4
        elseif instruction == 9 then
            local param1 = self:getParam(self.position + 1, mode1)
            self.relativeBase = self.relativeBase + param1
            self.position = self.position + 2
        else
            error("Unknown opcode: " .. instruction)
        end
    end
end

local function buildCompleteMap()
    local computer = Computer.new(data)
    local position = { x = 0, y = 0 }
    local map = { ["0,0"] = "." }
    local queue = { { pos = position, computer = computer } }
    local oxygenPos = nil
    while #queue > 0 do
        local current = table.remove(queue, 1)
        local pos = current.pos
        for dir = 1, 4 do
            local newPos = { x = pos.x + DIRECTIONS[dir].x, y = pos.y + DIRECTIONS[dir].y }
            local posKey = newPos.x .. "," .. newPos.y
            if not map[posKey] then
                local newComputer = current.computer:clone()
                local status = newComputer:run(dir)
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
    local computer = Computer.new(data)
    local position = { x = 0, y = 0 }
    local visited = { ["0,0"] = true }
    local queue = { { pos = position, steps = 0, computer = computer } }
    while #queue > 0 do
        local current = table.remove(queue, 1)
        local pos = current.pos
        for dir = 1, 4 do
            local newPos = { x = pos.x + DIRECTIONS[dir].x, y = pos.y + DIRECTIONS[dir].y }
            local posKey = newPos.x .. "," .. newPos.y
            if not visited[posKey] then
                local newComputer = current.computer:clone()
                local status = newComputer:run(dir)
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
                    local newKey = newPos.x .. "," .. newPos.y
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
