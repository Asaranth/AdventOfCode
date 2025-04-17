local utils = require("utils")

local function parseInput()
    local data = {}
    for value in utils.getInputData(5):gmatch("[^,]+") do
        table.insert(data, tonumber(value))
    end
    return data
end

local function getParameter(program, parameter, mode)
    if mode == 0 then
        return program[parameter + 1] or 0
    elseif mode == 1 then
        return parameter
    else
        error("Unknown paramter mode: " .. mode)
    end
end

local function runProgram(program, input)
    local pointer = 0
    local output = {}
    while true do
        local instruction = program[pointer + 1]
        local opcode = instruction % 100
        local mode1 = math.floor(instruction / 100) % 10
        local mode2 = math.floor(instruction / 1000) % 10
        if opcode == 99 then
            break
        elseif opcode == 1 or opcode == 2 then
            local param1 = getParameter(program, program[pointer + 2], mode1)
            local param2 = getParameter(program, program[pointer + 3], mode2)
            local dest = program[pointer + 4]
            if opcode == 1 then
                program[dest + 1] = param1 + param2
            else
                program[dest + 1] = param1 * param2
            end
            pointer = pointer + 4
        elseif opcode == 3 then
            local dest = program[pointer + 2]
            program[dest + 1] = input
            pointer = pointer + 2
        elseif opcode == 4 then
            local param1 = getParameter(program, program[pointer + 2], mode1)
            table.insert(output, param1)
            pointer = pointer + 2
        elseif opcode == 5 then
            local param1 = getParameter(program, program[pointer + 2], mode1)
            local param2 = getParameter(program, program[pointer + 3], mode2)
            if param1 ~= 0 then
                pointer = param2
            else
                pointer = pointer + 3
            end
        elseif opcode == 6 then
            local param1 = getParameter(program, program[pointer + 2], mode1)
            local param2 = getParameter(program, program[pointer + 3], mode2)
            if param1 == 0 then
                pointer = param2
            else
                pointer = pointer + 3
            end
        elseif opcode == 7 then
            local param1 = getParameter(program, program[pointer + 2], mode1)
            local param2 = getParameter(program, program[pointer + 3], mode2)
            local dest = program[pointer + 4]
            if param1 < param2 then
                program[dest + 1] = 1
            else
                program[dest + 1] = 0
            end
            pointer = pointer + 4
        elseif opcode == 8 then
            local param1 = getParameter(program, program[pointer + 2], mode1)
            local param2 = getParameter(program, program[pointer + 3], mode2)
            local dest = program[pointer + 4]
            if param1 == param2 then
                program[dest + 1] = 1
            else
                program[dest + 1] = 0
            end
            pointer = pointer + 4
        else
            error("Unknown opcode: " .. opcode)
        end
    end
    return output
end

local function solve(input)
    local program = parseInput()
    local result = runProgram(program, input)
    return result[#result]
end

print("Part One: " .. solve(1))
print("Part Two: " .. solve(5))
