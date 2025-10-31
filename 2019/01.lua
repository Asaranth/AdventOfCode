local utils = require("utils")

local data = {}

for line in utils.getInputData(1):gmatch("[^\r\n]+") do
    table.insert(data, tonumber(line))
end

local function solvePartOne()
    local total = 0
    for _, mass in ipairs(data) do
        total = total + (math.floor(mass / 3) - 2)
    end
    return total
end

local function solvePartTwo()
    local total = 0
    for _, mass in ipairs(data) do
        local fuel = math.floor(mass / 3) - 2
        while fuel > 0 do
            total = total + fuel
            fuel = math.floor(fuel / 3) - 2
        end
    end
    return total
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
