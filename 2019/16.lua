local utils = require("utils")

local function parseInput()
    local digits = {}
    for digit in utils.getInputData(16):gmatch("%d") do
        table.insert(digits, tonumber(digit))
    end
    return digits
end

local function getPattern(pos, length)
    local basePattern = { 0, 1, 0, -1 }
    local pattern = {}
    local idx = 1
    while #pattern < length + 1 do
        for _ = 1, pos do
            table.insert(pattern, basePattern[idx])
        end
        idx = idx % #basePattern + 1
    end
    table.remove(pattern, 1)
    local result = {}
    for i = 1, length do
        result[i] = pattern[i]
    end
    return result
end

local function processPhase(digits)
    local result = {}
    local len = #digits
    for i = 1, len do
        local pattern = getPattern(i, len)
        local sum = 0
        for j = 1, len do
            sum = sum + (digits[j] * pattern[j])
        end
        result[i] = math.abs(sum) % 10
    end
    return result
end

local function getOffset(digits)
    local offset = 0
    for i = 1, 7 do
        offset = offset * 10 + digits[i]
    end
    return offset
end

local function solvePartOne()
    local digits = parseInput()
    for _ = 1, 100 do
        digits = processPhase(digits)
    end
    local result = ""
    for i = 1, 8 do
        result = result .. digits[i]
    end
    return result
end

local function solvePartTwo()
    local digits = parseInput()
    local offset = getOffset(digits)
    local totalLen = #digits * 10000
    local relevantLen = totalLen - offset
    local fullDigits = {}
    for i = 1, relevantLen do
        local inputIndex = ((offset + i - 1) % #digits) + 1
        fullDigits[i] = digits[inputIndex]
    end
    for _ = 1, 100 do
        local sum = 0
        for i = #fullDigits, 1, -1 do
            sum = (sum + fullDigits[i]) % 10
            fullDigits[i] = sum
        end
    end
    local result = ""
    for i = 1, 8 do
        result = result .. fullDigits[i]
    end
    return result
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
