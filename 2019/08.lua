local utils = require("utils")

local data = utils.getInputData(8)
local width, height = 25, 6
local layerSize = width * height

local function getLayers()
    local layers = {}
    local cleanData = data:gsub("%s", ""):gsub("[^0-9]", "")
    for i = 1, #cleanData, layerSize do
        table.insert(layers, cleanData:sub(i, i + layerSize - 1))
    end
    return layers
end

local function countDigits(layer)
    local counts = { ['0'] = 0, ['1'] = 0, ['2'] = 0 }
    for i = 1, #layer do
        local digit = layer:sub(i, i)
        counts[digit] = counts[digit] + 1
    end
    return counts
end

local function solvePartOne()
    local layers = getLayers()
    local minZeroLayer = nil
    local minZeroCount = math.huge
    for _, layer in ipairs(layers) do
        local counts = countDigits(layer)
        if counts['0'] < minZeroCount then
            minZeroCount = counts['0']
            minZeroLayer = counts
        end
    end
    return minZeroLayer['1'] * minZeroLayer['2']
end

local function solvePartTwo()
    local layers = getLayers()
    local finalImage = {}
    for i = 1, layerSize do
        for _, layer in ipairs(layers) do
            local pixel = layer:sub(i, i)
            if pixel == '0' or pixel == '1' then
                finalImage[i] = pixel
                break
            end
        end
    end
    local result = {}
    for i = 1, #finalImage, width do
        local line = table.concat(finalImage, "", i, i + width - 1):gsub("0", " "):gsub("1", "█")
        table.insert(result, line)
    end
    return table.concat(result, "\n")
end

print("Part One: " .. solvePartOne())
print("Part Two:\n" .. solvePartTwo())
