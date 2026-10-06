--- Day 04: Secure Container
---
--- Validates six-digit password combinations meeting monotonic increase and adjacent duplicate criteria.

local utils = require("utils")

local data = utils.getInputData(4)
local rangeStart, rangeEnd = data:match("(%d+)-(%d+)")
rangeStart = tonumber(rangeStart)
rangeEnd = tonumber(rangeEnd)

--- Validates whether a password satisfies the monotonicity and adjacent matching digit constraints.
---
--- @param password number The numeric candidate password to test.
--- @param partTwo boolean When true, enforces that matching adjacent digits belong to a group of exactly two.
--- @return boolean True if the password meets all rules, false otherwise.
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

--- Counts valid password combinations in the puzzle input range for Part One.
---
--- @return number The number of valid passwords.
local function solvePartOne()
    local count = 0
    for password = rangeStart, rangeEnd do
        if isValidPassword(password, false) then
            count = count + 1
        end
    end
    return count
end

--- Counts valid password combinations in the puzzle input range for Part Two.
---
--- @return number The number of valid passwords with isolated duplicate pairs.
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
