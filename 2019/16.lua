--- Day 16: Flawed Frequency Transmission
---
--- Simulates Flawed Frequency Transmission (FFT) signal processing algorithms.

local utils = require("utils")

--- Parses the input text into a list of integer digits.
---
--- @return number[] Array of single-digit numbers.
local function parseInput()
    local digits = {}
    for digit in utils.getInputData(16):gmatch("%d") do
        table.insert(digits, tonumber(digit))
    end
    return digits
end

--- Generates the repeating transformation pattern for a specific output position.
---
--- @param pos number 1-based element output index.
--- @param length number Total length of the digit sequence.
--- @return number[] The truncated pattern values matching sequence length.
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

--- Applies a single phase of the standard FFT algorithm to all digits.
---
--- @param digits number[] Array of input digits.
--- @return number[] Array of transformed digits.
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

--- Reads the 7-digit message offset integer from the first seven input digits.
---
--- @param digits number[] Array of input digits.
--- @return number The parsed numeric message offset.
local function getOffset(digits)
    local offset = 0
    for i = 1, 7 do
        offset = offset * 10 + digits[i]
    end
    return offset
end

--- Solves Part One by running 100 phases of standard FFT and returning the first 8 digits.
---
--- @return string The first eight digits after 100 phases.
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

--- Solves Part Two using backward cumulative suffix sums on the 10,000x repeated signal.
---
--- @return string The eight-digit message starting at the message offset.
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
