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

local function intcodeComputer(program, inputs)
    local mem = { table.unpack(program) }
    local output = {}
    local ip = 1
    local inputIndex = 1
    local halted = false

    local function getParam(mode, offset)
        if mode == 0 then
            return mem[mem[ip + offset] + 1] or 0
        elseif mode == 1 then
            return mem[ip + offset] or 0
        end
    end
    local function run()
        while mem[ip] ~= 99 do
            local instruction = mem[ip]
            local opcode = instruction % 100
            local mode1 = math.floor(instruction / 100) % 10
            local mode2 = math.floor(instruction / 1000) % 10
            if opcode == 1 then
                local a, b, c = getParam(mode1, 1), getParam(mode2, 2), mem[ip + 3] + 1
                mem[c] = a + b
                ip = ip + 4
            elseif opcode == 2 then
                local a, b, c = getParam(mode1, 1), getParam(mode2, 2), mem[ip + 3] + 1
                mem[c] = a * b
                ip = ip + 4
            elseif opcode == 3 then
                if inputIndex > #inputs then
                    return false
                end
                local a = mem[ip + 1] + 1
                mem[a] = inputs[inputIndex]
                inputIndex = inputIndex + 1
                ip = ip + 2
            elseif opcode == 4 then
                local a = getParam(mode1, 1)
                table.insert(output, a)
                ip = ip + 2
                return a
            elseif opcode == 5 then
                local a, b = getParam(mode1, 1), getParam(mode2, 2)
                if a ~= 0 then
                    ip = b + 1
                else
                    ip = ip + 3
                end
            elseif opcode == 6 then
                local a, b = getParam(mode1, 1), getParam(mode2, 2)
                if a == 0 then
                    ip = b + 1
                else
                    ip = ip + 3
                end
            elseif opcode == 7 then
                local a, b, c = getParam(mode1, 1), getParam(mode2, 2), mem[ip + 3] + 1
                mem[c] = (a < b) and 1 or 0
                ip = ip + 4
            elseif opcode == 8 then
                local a, b, c = getParam(mode1, 1), getParam(mode2, 2), mem[ip + 3] + 1
                mem[c] = (a == b) and 1 or 0
                ip = ip + 4
            else
                error("Unknown opcode: " .. opcode)
            end
        end
        halted = true
        return nil
    end
    return {
        run = run,
        isHalted = function() return halted end,
        addInput = function(value) table.insert(inputs, value) end,
        getOutput = function() return table.remove(output, 1) end
    }
end

local function solvePartOne()
    local maxOutput = 0
    local phaseSettings = { 0, 1, 2, 3, 4 }
    local perms = {}
    permutations(phaseSettings, #phaseSettings, perms)
    for _, perm in ipairs(perms) do
        local signal = 0
        for _, phase in ipairs(perm) do
            local amp = intcodeComputer(data, { phase, signal })
            signal = amp.run()
        end
        maxOutput = math.max(maxOutput, signal)
    end
    return maxOutput
end

local function solvePartTwo()
    local maxOutput = 0
    local phaseSettings = { 5, 6, 7, 8, 9 }
    local perms = {}
    permutations(phaseSettings, #phaseSettings, perms)
    for _, perm in ipairs(perms) do
        local amplifiers = {}
        for i = 1, 5 do
            amplifiers[i] = intcodeComputer(data, { perm[i] })
        end
        local signal = 0
        local lastOutput = 0
        while true do
            for _, amp in ipairs(amplifiers) do
                amp.addInput(signal)
                local output = amp.run()
                if output then
                    signal = output
                end
            end
            if amplifiers[5].isHalted() then
                lastOutput = signal
                break
            end
        end
        maxOutput = math.max(maxOutput, lastOutput)
    end
    return maxOutput
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
