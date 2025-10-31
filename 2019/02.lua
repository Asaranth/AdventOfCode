local utils = require("utils")

local function parseInput()
    local data = {}

    for value in utils.getInputData(2):gmatch("[^,]+") do
        table.insert(data, tonumber(value))
    end
    return data
end

local function solvePartOne()
    local program = parseInput()
    program[2] = 12
    program[3] = 2
    local computer = utils.intcode(program)
    computer:run()
    return computer.memory[0]
end

local function solvePartTwo(targetOutput)
    local originalProgram = parseInput()
    for noun = 0, 99 do
        for verb = 0, 99 do
            local program = { table.unpack(originalProgram) }
            program[2] = noun
            program[3] = verb
            local computer = utils.intcode(program)
            computer:run()
            if computer.memory[0] == targetOutput then
                return 100 * noun + verb
            end
        end
    end
    error("No solution found")
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo(19690720))
