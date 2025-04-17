local utils = require("utils")

local data = {}
for line in utils.getInputData(9):gmatch("[^,]+") do
    table.insert(data, tonumber(line))
end

local function runIntcode(program, input)
    local memory = {}
    for i = 1, #program do
        memory[i - 1] = program[i]
    end
    local ip = 0
    local relativeBase = 0
    local output = {}
    local function read(addr)
        return memory[addr] or 0
    end
    local function write(addr, value)
        memory[addr] = value
    end
    local function getParam(mode, offset)
        local param = read(ip + offset)
        if mode == 0 then
            return read(param)
        elseif mode == 1 then
            return param
        elseif mode == 2 then
            return read(relativeBase + param)
        else
            error("Invalid parameter mode: " .. tostring(mode))
        end
    end
    local function getWriteAddr(mode, offset)
        local param = read(ip + offset)
        if mode == 0 then
            return param
        elseif mode == 2 then
            return relativeBase + param
        else
            error("Invalid parameter mode for write: " .. tostring(mode))
        end
    end
    while true do
        local instruction = read(ip)
        local opcode = instruction % 100
        local mode1 = math.floor(instruction / 100) % 10
        local mode2 = math.floor(instruction / 1000) % 10
        local mode3 = math.floor(instruction / 10000) % 10
        if opcode == 99 then
            break
        elseif opcode == 1 then
            local a = getParam(mode1, 1)
            local b = getParam(mode2, 2)
            local addr = getWriteAddr(mode3, 3)
            write(addr, a + b)
            ip = ip + 4
        elseif opcode == 2 then
            local a = getParam(mode1, 1)
            local b = getParam(mode2, 2)
            local addr = getWriteAddr(mode3, 3)
            write(addr, a * b)
            ip = ip + 4
        elseif opcode == 3 then
            local addr = getWriteAddr(mode1, 1)
            write(addr, table.remove(input, 1))
            ip = ip + 2
        elseif opcode == 4 then
            local val = getParam(mode1, 1)
            table.insert(output, val)
            ip = ip + 2
        elseif opcode == 5 then
            if getParam(mode1, 1) ~= 0 then
                ip = getParam(mode2, 2)
            else
                ip = ip + 3
            end
        elseif opcode == 6 then
            if getParam(mode1, 1) == 0 then
                ip = getParam(mode2, 2)
            else
                ip = ip + 3
            end
        elseif opcode == 7 then
            local a = getParam(mode1, 1)
            local b = getParam(mode2, 2)
            local addr = getWriteAddr(mode3, 3)
            write(addr, (a < b) and 1 or 0)
            ip = ip + 4
        elseif opcode == 8 then
            local a = getParam(mode1, 1)
            local b = getParam(mode2, 2)
            local addr = getWriteAddr(mode3, 3)
            write(addr, (a == b) and 1 or 0)
            ip = ip + 4
        elseif opcode == 9 then
            relativeBase = relativeBase + getParam(mode1, 1)
            ip = ip + 2
        else
            error("Unknown opcode: " .. tostring(opcode))
        end
    end
    return output
end

local function solve(input)
    local program = data
    local output = runIntcode(program, input)
    return output[1]
end

print("Part One: " .. solve({ 1 }))
print("Part Two: " .. solve({ 2 }))
