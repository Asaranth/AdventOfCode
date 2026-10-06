--- Day 13: Care Package
---
--- Simulates an arcade cabinet game running on Intcode and implements an automated joystick controller.

local utils = require("utils")

local data = {}
for value in utils.getInputData(13):gmatch("[^,]+") do
    table.insert(data, tonumber(value))
end

--- Runs the arcade cabinet to completion and counts the total number of block tiles rendered.
---
--- @return number The number of block tiles (tile ID 2) on the screen.
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

--- Plays the game by setting free play mode and tracking paddle/ball X positions for automated joystick inputs.
---
--- @return number The final score display value after all blocks are broken.
local function solvePartTwo()
    local score = 0
    local paddleX = 0
    local ballX = 0
    local computer = utils.intcode(data)
    computer.memory[0] = 2

    --- Determines joystick tilt based on the relative horizontal positions of paddle and ball.
    ---
    --- @return number Neutral (0), left (-1), or right (1).
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
