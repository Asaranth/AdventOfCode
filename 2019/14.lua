local utils = require("utils")

local function parseComponent(str)
    local quantity, chemical = str:match("(%d+)%s+(%w+)")
    return {
        chemical = chemical,
        quantity = tonumber(quantity)
    }
end

local function parseReaction(line)
    local ingredientsStr, resultStr = line:match("(.+)%s+=>%s+(.+)")
    local ingredients = {}
    for ingredient in ingredientsStr:gmatch("[^,]+") do
        local component = parseComponent(ingredient)
        ingredients[component.chemical] = component.quantity
    end
    local result = parseComponent(resultStr)
    return result.chemical, {
        quantity = result.quantity,
        ingredients = ingredients
    }
end

local recipes = {}
for line in utils.getInputData(14):gmatch("[^\r\n]+") do
    local chemical, recipe = parseReaction(line)
    recipes[chemical] = recipe
end

local function calculateOreRequirement(chemical, amount, leftovers)
    leftovers = leftovers or {}
    leftovers[chemical] = leftovers[chemical] or 0
    if chemical == "ORE" then
        return amount
    end
    if leftovers[chemical] >= amount then
        leftovers[chemical] = leftovers[chemical] - amount
        return 0
    end
    amount = amount - leftovers[chemical]
    leftovers[chemical] = 0
    local recipe = recipes[chemical]
    local batches = math.ceil(amount / recipe.quantity)
    local oreNeeded = 0
    for ingredient, qty in pairs(recipe.ingredients) do
        local totalNeeded = qty * batches
        oreNeeded = oreNeeded + calculateOreRequirement(ingredient, totalNeeded, leftovers)
    end
    leftovers[chemical] = (batches * recipe.quantity) - amount
    return oreNeeded
end

local function solvePartOne()
    return calculateOreRequirement("FUEL", 1)
end

local function solvePartTwo()
    local targetOre = 1000000000000
    local low = 0
    local high = targetOre
    local result = 0
    while low <= high do
        local mid = math.floor((low + high) / 2)
        local oreNeeded = calculateOreRequirement("FUEL", mid)
        if oreNeeded <= targetOre then
            result = mid
            low = mid + 1
        else
            high = mid - 1
        end
    end
    return result
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
