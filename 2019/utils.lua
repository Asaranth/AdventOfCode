local https = require("ssl.https")
local ltn12 = require("ltn12")

local function loadEnv()
    local envFile = io.open("../.env", "r")
    if not envFile then
        return {}
    end
    local envVars = {}
    for line in envFile:lines() do
        local key, value = tostring(line):match("^([^=]+)%s*=%s*(.+)$")
        if key and value then
            envVars[key] = value
        end
    end
    envFile:close()
    return envVars
end

local env = loadEnv()
local sessionCookie = env["AOC_SESSION_COOKIE"]

if not sessionCookie then
    error("AOC_SESSION_COOKIE not found in environment variables")
end

local function getInputData(day)
    local cacheFile = string.format("data/%02d.txt", day)
    local file = io.open(cacheFile, "r")
    if file then
        local data = file:read("*all")
        file:close()
        return data
    end
    local url = string.format("https://adventofcode.com/2019/day/%d/input", day)
    local response = {}
    local status = select(2, https.request {
        url = url,
        headers = { ["Cookie"] = "session=" .. sessionCookie },
        sink = ltn12.sink.table(response)
    })
    if status ~= 200 then
        error("Failed to fetch data. HTTP status: " .. tostring(status))
    end
    local data = table.concat(response)
    os.execute("mkdir -p data")
    local outputFile = io.open(cacheFile, "w")
    if outputFile == nil then
        error("Output File not found.")
    end
    outputFile:write(data)
    outputFile:close()
    return data
end

local IntcodeComputer = {}
IntcodeComputer.__index = IntcodeComputer

-- Create a new IntcodeComputer instance
function IntcodeComputer.new(program)
    local self = setmetatable({}, IntcodeComputer)
    self.memory = {}
    for i, v in ipairs(program) do
        self.memory[i - 1] = v
    end
    self.ip = 0  -- Instruction Pointer
    self.relativeBase = 0
    self.inputs = {}
    self.outputs = {}
    self.halted = false
    self.paused = false
    return self
end

-- Utility to read memory with default 0
function IntcodeComputer:getMemory(pos)
    return self.memory[pos] or 0
end

-- Utility to write to memory
function IntcodeComputer:setMemory(pos, val)
    self.memory[pos] = val
end

-- Get the parameter at a specific position, considering the mode
function IntcodeComputer:getParameter(mode, offset)
    local value = self:getMemory(self.ip + offset)
    if mode == 0 then
        return self:getMemory(value) -- Position mode
    elseif mode == 1 then
        return value -- Immediate mode
    elseif mode == 2 then
        return self:getMemory(value + self.relativeBase) -- Relative mode
    else
        error("Unknown parameter mode: " .. mode)
    end
end

-- Get the address for writing to memory, considering the mode
function IntcodeComputer:getWriteAddress(mode, offset)
    local value = self:getMemory(self.ip + offset)
    if mode == 2 then
        return value + self.relativeBase -- Relative mode
    else
        return value -- Position mode
    end
end

-- Add input to the computer
function IntcodeComputer:addInput(value)
    table.insert(self.inputs, value)
end

-- Retrieve the next output
function IntcodeComputer:getOutput()
    return table.remove(self.outputs, 1)
end

-- Main execution loop
function IntcodeComputer:run()
    self.paused = false
    while not self.halted and not self.paused do
        local instruction = self:getMemory(self.ip)
        local opcode = instruction % 100
        local mode1 = math.floor(instruction / 100) % 10
        local mode2 = math.floor(instruction / 1000) % 10
        local mode3 = math.floor(instruction / 10000) % 10

        if opcode == 99 then
            self.halted = true
            break
        elseif opcode == 1 then
            -- Add
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            local addr = self:getWriteAddress(mode3, 3)
            self:setMemory(addr, param1 + param2)
            self.ip = self.ip + 4
        elseif opcode == 2 then
            -- Multiply
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            local addr = self:getWriteAddress(mode3, 3)
            self:setMemory(addr, param1 * param2)
            self.ip = self.ip + 4
        elseif opcode == 3 then
            -- Input
            if #self.inputs == 0 then
                self.paused = true -- Pause if no input is available
                break
            end
            local addr = self:getWriteAddress(mode1, 1)
            self:setMemory(addr, table.remove(self.inputs, 1))
            self.ip = self.ip + 2
        elseif opcode == 4 then
            -- Output
            local param1 = self:getParameter(mode1, 1)
            table.insert(self.outputs, param1)
            self.ip = self.ip + 2
        elseif opcode == 5 then
            -- Jump-if-true
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            if param1 ~= 0 then
                self.ip = param2
            else
                self.ip = self.ip + 3
            end
        elseif opcode == 6 then
            -- Jump-if-false
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            if param1 == 0 then
                self.ip = param2
            else
                self.ip = self.ip + 3
            end
        elseif opcode == 7 then
            -- Less than
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            local addr = self:getWriteAddress(mode3, 3)
            self:setMemory(addr, param1 < param2 and 1 or 0)
            self.ip = self.ip + 4
        elseif opcode == 8 then
            -- Equals
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            local addr = self:getWriteAddress(mode3, 3)
            self:setMemory(addr, param1 == param2 and 1 or 0)
            self.ip = self.ip + 4
        elseif opcode == 9 then
            -- Adjust relative base
            local param1 = self:getParameter(mode1, 1)
            self.relativeBase = self.relativeBase + param1
            self.ip = self.ip + 2
        else
            error("Unknown opcode: " .. opcode)
        end
    end
end

-- Check if the computer has halted
function IntcodeComputer:isHalted()
    return self.halted
end

return { getInputData = getInputData, intcode = IntcodeComputer.new}