local utils = require("utils")

local data = {}
for value in utils.getInputData(13):gmatch("[^,]+") do
    table.insert(data, tonumber(value))
end

local function copyProgram(program)
    local copy = {}
    for i, v in ipairs(program) do copy[i - 1] = v end
    return copy
end

local function runProgram(program, getInput)
    local memory = copyProgram(program)
    local ip = 0
    local relativeBase = 0
    memory[0] = 2

    local function getParamValue(param, mode)
        mode = mode or 0
        if mode == 0 then
            return memory[param] or 0
        elseif mode == 1 then
            return param or 0
        elseif mode == 2 then
            return memory[param + relativeBase] or 0
        end
    end

    local function getWriteAddress(param, mode)
        mode = mode or 0
        if mode == 2 then return param + relativeBase end
        return param
    end

    while true do
        local opcode = memory[ip]
        if not opcode then break end
        local instruction = opcode % 100
        local modes = {
            math.floor(opcode / 100) % 10,
            math.floor(opcode / 1000) % 10,
            math.floor(opcode / 10000) % 10
        }
        if instruction == 99 then
            break
        elseif instruction == 1 then
            local p1 = getParamValue(memory[ip + 1], modes[1])
            local p2 = getParamValue(memory[ip + 2], modes[2])
            local addr = getWriteAddress(memory[ip + 3], modes[3])
            memory[addr] = p1 + p2
            ip = ip + 4
        elseif instruction == 2 then
            local p1 = getParamValue(memory[ip + 1], modes[1])
            local p2 = getParamValue(memory[ip + 2], modes[2])
            local addr = getWriteAddress(memory[ip + 3], modes[3])
            memory[addr] = p1 * p2
            ip = ip + 4
        elseif instruction == 3 then
            local addr = getWriteAddress(memory[ip + 1], modes[1])
            memory[addr] = getInput()
            ip = ip + 2
        elseif instruction == 4 then
            local value = getParamValue(memory[ip + 1], modes[1])
            coroutine.yield(value)
            ip = ip + 2
        elseif instruction == 5 then
            local p1 = getParamValue(memory[ip + 1], modes[1])
            local p2 = getParamValue(memory[ip + 2], modes[2])
            ip = p1 ~= 0 and p2 or ip + 3
        elseif instruction == 6 then
            local p1 = getParamValue(memory[ip + 1], modes[1])
            local p2 = getParamValue(memory[ip + 2], modes[2])
            ip = p1 == 0 and p2 or ip + 3
        elseif instruction == 7 then
            local p1 = getParamValue(memory[ip + 1], modes[1])
            local p2 = getParamValue(memory[ip + 2], modes[2])
            local addr = getWriteAddress(memory[ip + 3], modes[3])
            memory[addr] = p1 < p2 and 1 or 0
            ip = ip + 4
        elseif instruction == 8 then
            local p1 = getParamValue(memory[ip + 1], modes[1])
            local p2 = getParamValue(memory[ip + 2], modes[2])
            local addr = getWriteAddress(memory[ip + 3], modes[3])
            memory[addr] = p1 == p2 and 1 or 0
            ip = ip + 4
        elseif instruction == 9 then
            local p1 = getParamValue(memory[ip + 1], modes[1])
            relativeBase = relativeBase + p1
            ip = ip + 2
        else
            error("Unknown opcode: " .. opcode)
        end
    end
end

local function solvePartOne()
    local outputs = {}
    local co = coroutine.create(function()
        runProgram(data, function() return 0 end)
    end)
    while coroutine.status(co) ~= "dead" do
        local success, value = coroutine.resume(co)
        if success and value then table.insert(outputs, value) end
    end
    local blockCount = 0
    for i = 1, #outputs, 3 do
        local tileId = outputs[i + 2]
        if tileId == 2 then blockCount = blockCount + 1 end
    end
    return blockCount
end

local function solvePartTwo()
    local score = 0
    local paddleX = 0
    local ballX = 0

    local function getJoystickPosition()
        if paddleX < ballX then
            return 1
        elseif paddleX > ballX then
            return -1
        else
            return 0
        end
    end

    local co = coroutine.create(function()
        runProgram(data, getJoystickPosition)
    end)
    while coroutine.status(co) ~= "dead" do
        local success, x = coroutine.resume(co)
        if not success or not x then break end
        success, y = coroutine.resume(co)
        if not success or not y then break end
        success, value = coroutine.resume(co)
        if not success or not value then break end
        if x == -1 and y == 0 then
            score = value
        else
            if value == 3 then
                paddleX = x
            elseif value == 4 then
                ballX = x
            end
        end
    end
    return score
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
