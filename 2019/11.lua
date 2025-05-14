local utils = require("utils")

local function parseInput()
    local data = {}
    for value in utils.getInputData(11):gmatch("[^,]+") do
        table.insert(data, tonumber(value))
    end
    return data
end

local DIRECTIONS = { { 0, 1 }, { 1, 0 }, { 0, -1 }, { -1, 0 } }

local function runRobot(startOnWhite)
    local program = parseInput()
    local computer = utils.intcode(program)
    local position = { x = 0, y = 0 }
    local direction = 1
    local panels = {}
    local paintedPanels = {}

    local function getPanelColor(pos)
        return panels[pos.x .. "," .. pos.y] or 0
    end

    local function setPanelColor(pos, color)
        panels[pos.x .. "," .. pos.y] = color
    end

    if startOnWhite then
        setPanelColor(position, 1)
    end

    while not computer:isHalted() do
        computer:addInput(getPanelColor(position))
        computer:run()
        local paintColor = computer:getOutput()
        local turnDirection = computer:getOutput()
        if not paintColor or not turnDirection then
            break
        end
        setPanelColor(position, paintColor)
        paintedPanels[position.x .. "," .. position.y] = true
        if turnDirection == 0 then
            direction = (direction - 2) % #DIRECTIONS + 1
        elseif turnDirection == 1 then
            direction = direction % #DIRECTIONS + 1
        end
        position.x = position.x + DIRECTIONS[direction][1]
        position.y = position.y + DIRECTIONS[direction][2]
    end
    return panels, paintedPanels
end

local function solvePartOne()
    local _, paintedPanels = runRobot(false)
    local count = 0
    for _ in pairs(paintedPanels) do
        count = count + 1
    end
    return count
end

local function solvePartTwo()
    local panels = runRobot(true)
    local minX, minY, maxX, maxY = 0, 0, 0, 0
    for key in pairs(panels) do
        local x, y = key:match("(-?%d+),(-?%d+)")
        x, y = tonumber(x), tonumber(y)
        minX, maxX = math.min(minX, x), math.max(maxX, x)
        minY, maxY = math.min(minY, y), math.max(maxY, y)
    end
    local grid = {}
    for y = maxY, minY, -1 do
        local row = {}
        for x = minX, maxX do
            row[#row + 1] = panels[x .. "," .. y] == 1 and "#" or " "
        end
        table.insert(grid, table.concat(row))
    end
    return table.concat(grid, "\n")
end

print("Part One: " .. solvePartOne())
print("Part Two:\n" .. solvePartTwo())
