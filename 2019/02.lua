local utils = require("utils")

local function parseInput()
    local data = {}
    for value in utils.getInputData(2):gmatch("[^,]+") do
        table.insert(data, tonumber(value))
    end
    return data
end

local function runIntcode(program)
    local index = 1
    while program[index] ~= 99 do
        local opcode = program[index]
        local param1 = program[index + 1] + 1
        local param2 = program[index + 2] + 1
        local param3 = program[index + 3] + 1
        if opcode == 1 then
            program[param3] = program[param1] + program[param2]
        elseif opcode == 2 then
            program[param3] = program[param1] * program[param2]
        else
            error("Unknown opcode: " .. opcode)
        end
        index = index + 4
    end
    return program
end

local function solvePartOne()
    local program = parseInput()
    program[2] = 12
    program[3] = 2
    local finalProgram = runIntcode(program)
    return finalProgram[1]
end

local function solvePartTwo(targetOutput)
    local originalProgram = parseInput()
    for noun = 0, 99 do
        for verb = 0, 99 do
            local program = { table.unpack(originalProgram) }
            program[2] = noun
            program[3] = verb
            local result = runIntcode(program)
            if result[1] == targetOutput then
                return 100 * noun + verb
            end
        end
    end
    error("No solution found")
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo(19690720))
