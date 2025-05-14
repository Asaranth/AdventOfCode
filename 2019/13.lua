local utils = require("utils")

local data = {}
for value in utils.getInputData(13):gmatch("[^,]+") do
    table.insert(data, tonumber(value))
end

local function solvePartOne()
    local computer = utils.intcode(data)
    local blockCount = 0
    while not computer:isHalted() do
        computer:run()
        while #computer.outputs >= 3 do
            local _ = computer:getOutput()
            local _ = computer:getOutput()
            local tileId = computer:getOutput()
            if tileId == 2 then
                blockCount = blockCount + 1
            end
        end
    end
    return blockCount
end

local function solvePartTwo()
    local score = 0
    local paddleX = 0
    local ballX = 0
    local computer = utils.intcode(data)
    computer.memory[0] = 2

    local function getJoystickPosition()
        if paddleX < ballX then
            return 1
        elseif paddleX > ballX then
            return -1
        else
            return 0
        end
    end

    while not computer:isHalted() do
        if #computer.inputs == 0 then
            computer:addInput(getJoystickPosition())
        end
        computer:run()
        while #computer.outputs >= 3 do
            local x = computer:getOutput()
            local y = computer:getOutput()
            local value = computer:getOutput()

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
    end
    return score
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
