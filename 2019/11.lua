local utils = require("utils")
local data = {}
for value in utils.getInputData(11):gmatch("[^,]+") do
    table.insert(data, tonumber(value))
end

local directions = {
    [0] = { 0, 1 },
    [1] = { 1, 0 },
    [2] = { 0, -1 },
    [3] = { -1, 0 }
}

local function createComputer(program)
    local comp = { memory = {}, ip = 0, relativeBase = 0, halted = false }
    for i, v in ipairs(program) do comp.memory[i - 1] = v end

    function comp:get(pos)
        return self.memory[pos] or 0
    end

    function comp:set(pos, val)
        self.memory[pos] = val
    end

    function comp:getParameterMode(instruction, paramNum)
        return math.floor(instruction / (10 * (10 ^ paramNum))) % 10
    end

    function comp:getParameter(instruction, paramNum)
        local mode = self:getParameterMode(instruction, paramNum)
        local value = self:get(self.ip + paramNum)
        if mode == 0 then
            return self:get(value)
        elseif mode == 1 then
            return value
        else
            return self:get(value + self.relativeBase)
        end
    end

    function comp:getAddress(instruction, paramNum)
        local mode = self:getParameterMode(instruction, paramNum)
        local value = self:get(self.ip + paramNum)
        return mode == 2 and value + self.relativeBase or value
    end

    function comp:run(input)
        while true do
            local instruction = self:get(self.ip)
            local opcode = instruction % 100
            if opcode == 99 then
                self.halted = true
                return nil
            end
            if opcode == 1 then
                local p1 = self:getParameter(instruction, 1)
                local p2 = self:getParameter(instruction, 2)
                local p3 = self:getAddress(instruction, 3)
                self:set(p3, p1 + p2)
                self.ip = self.ip + 4
            elseif opcode == 2 then
                local p1 = self:getParameter(instruction, 1)
                local p2 = self:getParameter(instruction, 2)
                local p3 = self:getAddress(instruction, 3)
                self:set(p3, p1 * p2)
                self.ip = self.ip + 4
            elseif opcode == 3 then
                if input == nil then return nil end
                local p1 = self:getAddress(instruction, 1)
                self:set(p1, input)
                input = nil
                self.ip = self.ip + 2
            elseif opcode == 4 then
                local p1 = self:getParameter(instruction, 1)
                self.ip = self.ip + 2
                return p1
            elseif opcode == 5 then
                local p1 = self:getParameter(instruction, 1)
                local p2 = self:getParameter(instruction, 2)
                self.ip = p1 ~= 0 and p2 or self.ip + 3
            elseif opcode == 6 then
                local p1 = self:getParameter(instruction, 1)
                local p2 = self:getParameter(instruction, 2)
                self.ip = p1 == 0 and p2 or self.ip + 3
            elseif opcode == 7 then
                local p1 = self:getParameter(instruction, 1)
                local p2 = self:getParameter(instruction, 2)
                local p3 = self:getAddress(instruction, 3)
                self:set(p3, p1 < p2 and 1 or 0)
                self.ip = self.ip + 4
            elseif opcode == 8 then
                local p1 = self:getParameter(instruction, 1)
                local p2 = self:getParameter(instruction, 2)
                local p3 = self:getAddress(instruction, 3)
                self:set(p3, p1 == p2 and 1 or 0)
                self.ip = self.ip + 4
            elseif opcode == 9 then
                local p1 = self:getParameter(instruction, 1)
                self.relativeBase = self.relativeBase + p1
                self.ip = self.ip + 2
            end
        end
    end

    return comp
end

local function handleRobot(robot)
    local computer = createComputer(data)
    while not computer.halted do
        local color = computer:run(robot.panels[robot.x .. "," .. robot.y] or 0)
        if color == nil then break end
        local turn = computer:run()
        if turn == nil then break end
        robot.panels[robot.x .. "," .. robot.y] = color
        robot.direction = (robot.direction + (turn == 0 and -1 or 1)) % 4
        local dir = directions[robot.direction]
        robot.x = robot.x + dir[1]
        robot.y = robot.y + dir[2]
    end
    return robot
end

local function solvePartOne()
    local robot = handleRobot({ x = 0, y = 0, direction = 0, panels = {} })
    local count = 0
    for _ in pairs(robot.panels) do count = count + 1 end
    return count
end

local function solvePartTwo()
    local robot = handleRobot({ x = 0, y = 0, direction = 0, panels = { ["0,0"] = 1 } })
    local minX, maxX = math.huge, -math.huge
    local minY, maxY = math.huge, -math.huge
    for key in pairs(robot.panels) do
        local x, y = string.match(key, "(-?%d+),(-?%d+)")
        x, y = tonumber(x), tonumber(y)
        minX = math.min(minX, x)
        maxX = math.max(maxX, x)
        minY = math.min(minY, y)
        maxY = math.max(maxY, y)
    end
    local output = "\n"
    for y = maxY, minY, -1 do
        local line = ""
        for x = minX, maxX do line = line .. (robot.panels[x .. "," .. y] == 1 and "#" or " ") end
        output = output .. line .. "\n"
    end
    return output
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
