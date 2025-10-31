local utils = require("utils")

local data = {}
for line in utils.getInputData(22):gmatch("[^\r\n]+") do
    table.insert(data, line)
end

local function modMult(a, b, m)
    a = a % m
    b = b % m
    if a == 0 or b == 0 then
        return 0
    end
    local threshold = math.floor(math.sqrt(m))
    if a < threshold and b < threshold then
        return (a * b) % m
    end
    local result = 0
    a = a % m
    while b > 0 do
        if b & 1 == 1 then
            result = (result + a) % m
        end
        a = (a * 2) % m
        b = b >> 1
    end
    return result
end

local function modPow(base, exp, m)
    if exp == 0 then
        return 1
    end
    local result = 1
    base = base % m
    while exp > 0 do
        if exp & 1 == 1 then
            result = modMult(result, base, m)
        end
        exp = exp >> 1
        base = modMult(base, base, m)
    end
    return result
end

local function modInverse(p, modulus)
    return modPow(p, modulus - 2, modulus)
end

local function fCompose(f, g, modulus)
    local a, b = f[1], f[2]
    local c, d = g[1], g[2]
    local new_a = modMult(c, a, modulus)
    local new_b = (modMult(c, b, modulus) + d) % modulus
    return { new_a, new_b }
end

local function fApply(f, x, modulus)
    local a, b = f[1], f[2]
    return (modMult(a, x, modulus) + b) % modulus
end

local function fRepeat(f, n, modulus)
    if n == 0 then
        return { 1, 0 }
    end
    if n == 1 then
        return { f[1], f[2] }
    end
    local half = n >> 1
    local odd = n & 1
    local g = fRepeat(f, half, modulus)
    local gg = fCompose(g, g, modulus)
    if odd == 1 then
        return fCompose(f, gg, modulus)
    else
        return gg
    end
end

local function fInverse(instructions, count)
    local result = { 1, 0 }
    for i = #instructions, 1, -1 do
        local line = instructions[i]
        local f
        if line == "deal into new stack" then
            f = { count - 1, count - 1 }
        elseif line:match("^cut") then
            local amount = tonumber(line:match("cut (.+)"))
            f = { 1, amount }
        else
            local increment = tonumber(line:match("deal with increment (.+)"))
            f = { modInverse(increment, count), 0 }
        end
        result = fCompose(result, f, count)
    end
    return result
end

local function solvePartOne()
    local deckSize = 10007
    local index = 2019
    for _, instruction in ipairs(data) do
        if instruction == "deal into new stack" then
            index = deckSize - 1 - index
        elseif instruction:match("^cut") then
            local n = tonumber(instruction:match("cut (.+)"))
            index = index - n
            if index < 0 then
                index = index + deckSize
            end
            if index >= deckSize then
                index = index - deckSize
            end
        elseif instruction:match("^deal with increment") then
            local n = tonumber(instruction:match("deal with increment (.+)"))
            index = (index * n) % deckSize
        end
    end
    return index
end

local function solvePartTwo()
    local deckSize = 119315717514047
    local iterations = 101741582076661
    local index = 2020
    local f = fInverse(data, deckSize)
    local fn = fRepeat(f, iterations, deckSize)
    return fApply(fn, index, deckSize)
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())