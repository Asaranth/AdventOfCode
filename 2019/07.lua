local utils = require("utils")

local data = {}
for value in utils.getInputData(7):gmatch("[^,]+") do
    table.insert(data, tonumber(value))
end

local function permutations(array, n, results)
    if n == 1 then
        table.insert(results, { table.unpack(array) })
        return
    end
    for i = 1, n do
        permutations(array, n - 1, results)
        if n % 2 == 1 then
            array[1], array[n] = array[n], array[1]
        else
            array[i], array[n] = array[n], array[i]
        end
    end
end

local function runAmplifier(program, phaseSetting, inputSignal)
    local computer = utils.intcode(program)
    computer:addInput(phaseSetting)
    computer:addInput(inputSignal)
    computer:run()
    return computer:getOutput()
end

local function solvePartOne()
    local maxOutput = 0
    local phaseSettings = { 0, 1, 2, 3, 4 }
    local permutationsList = {}
    permutations(phaseSettings, #phaseSettings, permutationsList)
    for _, perm in ipairs(permutationsList) do
        local signal = 0
        for _, phase in ipairs(perm) do
            signal = runAmplifier(data, phase, signal)
        end
        maxOutput = math.max(maxOutput, signal)
    end
    return maxOutput
end

local function solvePartTwo()
    local maxOutput = 0
    local phaseSettings = { 5, 6, 7, 8, 9 }
    local permutationsList = {}
    permutations(phaseSettings, #phaseSettings, permutationsList)
    for _, perm in ipairs(permutationsList) do
        local amplifiers = {}
        for i = 1, 5 do
            amplifiers[i] = utils.intcode(data)
            amplifiers[i]:addInput(perm[i])
        end
        local signal = 0
        local done = false
        while not done do
            for i, amp in ipairs(amplifiers) do
                amp:addInput(signal)
                amp:run()
                local output = amp:getOutput()
                if output then
                    signal = output
                end
                if amp:isHalted() and i == 5 then
                    done = true
                end
            end
        end
        maxOutput = math.max(maxOutput, signal)
    end
    return maxOutput
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
