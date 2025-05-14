local utils = require("utils")

local function parseInput()
    local data = {}
    for value in utils.getInputData(5):gmatch("[^,]+") do
        table.insert(data, tonumber(value))
    end
    return data
end

local function solve(input)
    local program = parseInput()
    local computer = utils.intcode(program)
    computer:addInput(input)
    computer:run()
    local output
    while true do
        local value = computer:getOutput()
        if not value then break end
        output = value
    end
    return output
end

print("Part One: " .. solve(1))
print("Part Two: " .. solve(5))
