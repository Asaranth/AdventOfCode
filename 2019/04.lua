local utils = require("utils")

local data = utils.getInputData(4)
local rangeStart, rangeEnd = data:match("(%d+)-(%d+)")
rangeStart = tonumber(rangeStart)
rangeEnd = tonumber(rangeEnd)

local function isValidPassword(password, partTwo)
    local passwordStr = tostring(password)
    local hasAdjacent = false
    local neverDecreases = true
    local counts = {}
    for i = 1, #passwordStr - 1 do
        if tonumber(passwordStr:sub(i, i)) > tonumber(passwordStr:sub(i + 1, i + 1)) then
            neverDecreases = false
            break
        end
    end
    for digit in passwordStr:gmatch("%d") do
        counts[digit] = (counts[digit] or 0) + 1
    end
    if partTwo then
        for _, count in pairs(counts) do
            if count == 2 then
                hasAdjacent = true
                break
            end
        end
    else
        for i = 1, #passwordStr - 1 do
            if passwordStr:sub(i, i) == passwordStr:sub(i + 1, i + 1) then
                hasAdjacent = true
                break
            end
        end
    end
    return hasAdjacent and neverDecreases
end

local function solvePartOne()
    local count = 0
    for password = rangeStart, rangeEnd do
        if isValidPassword(password, false) then
            count = count + 1
        end
    end
    return count
end

local function solvePartTwo()
    local count = 0
    for password = rangeStart, rangeEnd do
        if isValidPassword(password, true) then
            count = count + 1
        end
    end
    return count
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
