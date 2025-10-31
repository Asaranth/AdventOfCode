local utils = require("utils")

local function parseInput()
    local data = {}
    for value in utils.getInputData(19):gmatch("[^,]+") do
        table.insert(data, tonumber(value))
    end
    return data
end

local function checkPosition(program, x, y)
    local computer = utils.intcode(program)
    computer:addInput(x)
    computer:addInput(y)
    computer:run()
    return computer:getOutput()
end

local function solvePartOne()
    local program = parseInput()
    local count = 0
    for y = 0, 49 do
        for x = 0, 49 do
            local result = checkPosition(program, x, y)
            if result == 1 then
                count = count + 1
            end
        end
    end
    return count
end

local function solvePartTwo()
    local program = parseInput()
    local size = 100
    local y = size
    local x = 0
    while checkPosition(program, x, y) == 0 do
        x = x + 1
    end
    while true do
        local topLeftY = y - size + 1
        if topLeftY >= 0 and checkPosition(program, x, topLeftY) == 1 then
            if checkPosition(program, x + size - 1, topLeftY) == 1 then
                return x * 10000 + topLeftY
            end
        end
        y = y + 1
        while checkPosition(program, x, y) == 0 do
            x = x + 1
        end
    end
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())