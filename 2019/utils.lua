--- Common Utilities and Intcode Virtual Machine
---
--- Provides input data fetching, caching, and the Intcode computer interpreter.

local https = require("ssl.https")
local ltn12 = require("ltn12")

--- Loads environment variables from the parent .env file.
---
--- @return table<string, string> A key-value table of environment variables.
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

--- Retrieves the puzzle input data for a specified day, caching it locally in the data directory.
---
--- @param day number The day of the puzzle (1-25).
--- @return string The raw puzzle input string.
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

--- @class IntcodeComputer
--- @field memory table<number, number> Addressable memory containing instruction opcodes and values.
--- @field ip number Instruction pointer indicating current execution position.
--- @field relativeBase number Relative base offset for relative parameter addressing mode.
--- @field inputs number[] FIFO queue of pending input values.
--- @field outputs number[] FIFO queue of emitted output values.
--- @field halted boolean Flag indicating whether the computer encountered opcode 99 and halted.
--- @field paused boolean Flag indicating whether execution is paused awaiting input.
local IntcodeComputer = {}
IntcodeComputer.__index = IntcodeComputer

--- Initialises a new Intcode computer instance with a given program.
---
--- @param program number[] List of integer instructions forming the initial memory state.
--- @return IntcodeComputer A newly initialised Intcode computer instance.
function IntcodeComputer.new(program)
    local self = setmetatable({}, IntcodeComputer)
    self.memory = {}
    for i, v in ipairs(program) do
        self.memory[i - 1] = v
    end
    self.ip = 0
    self.relativeBase = 0
    self.inputs = {}
    self.outputs = {}
    self.halted = false
    self.paused = false
    return self
end

--- Retrieves the value at the specified memory address.
---
--- @param pos number Zero-based memory address.
--- @return number The value stored at the address, defaulting to 0 if uninitialised.
function IntcodeComputer:getMemory(pos)
    return self.memory[pos] or 0
end

--- Stores a value at the specified memory address.
---
--- @param pos number Zero-based memory address.
--- @param val number Value to write.
function IntcodeComputer:setMemory(pos, val)
    self.memory[pos] = val
end

--- Evaluates the parameter value for an instruction based on its parameter mode.
---
--- @param mode number Mode flag (0: position mode, 1: immediate mode, 2: relative mode).
--- @param offset number Offset relative to the current instruction pointer.
--- @return number The resolved parameter value.
function IntcodeComputer:getParameter(mode, offset)
    local value = self:getMemory(self.ip + offset)
    if mode == 0 then
        return self:getMemory(value)
    elseif mode == 1 then
        return value
    elseif mode == 2 then
        return self:getMemory(value + self.relativeBase)
    else
        error("Unknown parameter mode: " .. mode)
    end
end

--- Resolves the write destination address based on parameter mode.
---
--- @param mode number Mode flag (0: position mode, 2: relative mode).
--- @param offset number Offset relative to the current instruction pointer.
--- @return number The resolved memory write address.
function IntcodeComputer:getWriteAddress(mode, offset)
    local value = self:getMemory(self.ip + offset)
    if mode == 2 then
        return value + self.relativeBase
    else
        return value
    end
end

--- Appends an input value to the computer's input queue.
---
--- @param value number Input value to queue.
function IntcodeComputer:addInput(value)
    table.insert(self.inputs, value)
end

--- Retrieves and removes the oldest output value from the output queue.
---
--- @return number|nil The next output value, or nil if the output queue is empty.
function IntcodeComputer:getOutput()
    return table.remove(self.outputs, 1)
end

--- Executes instructions until the program halts or pauses waiting for input.
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
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            local addr = self:getWriteAddress(mode3, 3)
            self:setMemory(addr, param1 + param2)
            self.ip = self.ip + 4
        elseif opcode == 2 then
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            local addr = self:getWriteAddress(mode3, 3)
            self:setMemory(addr, param1 * param2)
            self.ip = self.ip + 4
        elseif opcode == 3 then
            if #self.inputs == 0 then
                self.paused = true
                break
            end
            local addr = self:getWriteAddress(mode1, 1)
            self:setMemory(addr, table.remove(self.inputs, 1))
            self.ip = self.ip + 2
        elseif opcode == 4 then
            local param1 = self:getParameter(mode1, 1)
            table.insert(self.outputs, param1)
            self.ip = self.ip + 2
        elseif opcode == 5 then
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            if param1 ~= 0 then
                self.ip = param2
            else
                self.ip = self.ip + 3
            end
        elseif opcode == 6 then
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            if param1 == 0 then
                self.ip = param2
            else
                self.ip = self.ip + 3
            end
        elseif opcode == 7 then
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            local addr = self:getWriteAddress(mode3, 3)
            self:setMemory(addr, param1 < param2 and 1 or 0)
            self.ip = self.ip + 4
        elseif opcode == 8 then
            local param1 = self:getParameter(mode1, 1)
            local param2 = self:getParameter(mode2, 2)
            local addr = self:getWriteAddress(mode3, 3)
            self:setMemory(addr, param1 == param2 and 1 or 0)
            self.ip = self.ip + 4
        elseif opcode == 9 then
            local param1 = self:getParameter(mode1, 1)
            self.relativeBase = self.relativeBase + param1
            self.ip = self.ip + 2
        else
            error("Unknown opcode: " .. opcode)
        end
    end
end

--- Checks whether the Intcode computer has completed execution.
---
--- @return boolean True if the computer has halted on opcode 99, false otherwise.
function IntcodeComputer:isHalted()
    return self.halted
end

return { getInputData = getInputData, intcode = IntcodeComputer.new }
