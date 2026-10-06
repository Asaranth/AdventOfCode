--- Day 14: Space Stoichiometry
---
--- Calculates raw ORE requirements and fuel yields using chemical reaction recipes and binary search.

local utils = require("utils")

--- Parses a quantity and chemical identifier from a component string (e.g. "10 ORE").
---
--- @param str string The raw component string.
--- @return { chemical: string, quantity: number } The parsed chemical component.
local function parseComponent(str)
    local quantity, chemical = str:match("(%d+)%s+(%w+)")
    return {
        chemical = chemical,
        quantity = tonumber(quantity)
    }
end

--- Parses a single chemical reaction equation line.
---
--- @param line string The raw reaction string (e.g. "7 A, 1 E => 1 FUEL").
--- @return string resultChemical Name of the produced chemical.
--- @return { quantity: number, ingredients: table<string, number> } The recipe details.
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

--- Recursively calculates the ORE needed to produce a specified quantity of chemical, reusing surplus leftovers.
---
--- @param chemical string Name of the chemical to produce.
--- @param amount number Target quantity required.
--- @param leftovers table<string, number>|nil Reusable surplus quantities from earlier reactions.
--- @return number Total units of ORE consumed.
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

--- Calculates the minimum ORE required to produce 1 unit of FUEL for Part One.
---
--- @return number The required ORE quantity.
local function solvePartOne()
    return calculateOreRequirement("FUEL", 1)
end

--- Calculates the maximum FUEL producible with 1 trillion (10^12) ORE using binary search for Part Two.
---
--- @return number Maximum units of FUEL.
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
