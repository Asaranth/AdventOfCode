local utils = require("utils")

local data = {}
for line in utils.getInputData(9):gmatch("[^,]+") do
    table.insert(data, tonumber(line))
end

local function solve(input)
    local computer = utils.intcode(data)
    for _, val in ipairs(input) do
        computer:addInput(val)
    end
    computer:run()
    return computer:getOutput()
end

print("Part One: " .. solve({ 1 }))
print("Part Two: " .. solve({ 2 }))
