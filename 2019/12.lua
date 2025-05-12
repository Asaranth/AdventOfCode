local utils = require("utils")

local data = {}
for line in utils.getInputData(12):gmatch("[^\r\n]+") do
    local x, y, z = line:match("<x=(-?%d+), y=(-?%d+), z=(-?%d+)>")
    table.insert(data, {
        pos = { x = tonumber(x), y = tonumber(y), z = tonumber(z) },
        vel = { x = 0, y = 0, z = 0 }
    })
end

local function applyGravity(m1, m2)
    local axes = { "x", "y", "z" }
    for _, axis in ipairs(axes) do
        if m1.pos[axis] < m2.pos[axis] then
            m1.vel[axis] = m1.vel[axis] + 1
            m2.vel[axis] = m2.vel[axis] - 1
        elseif m1.pos[axis] > m2.pos[axis] then
            m1.vel[axis] = m1.vel[axis] - 1
            m2.vel[axis] = m2.vel[axis] + 1
        end
    end
end

local function applyVelocity(moon)
    moon.pos.x = moon.pos.x + moon.vel.x
    moon.pos.y = moon.pos.y + moon.vel.y
    moon.pos.z = moon.pos.z + moon.vel.z
end

local function calculateEnergy(moon)
    local potential = math.abs(moon.pos.x) + math.abs(moon.pos.y) + math.abs(moon.pos.z)
    local kinetic = math.abs(moon.vel.x) + math.abs(moon.vel.y) + math.abs(moon.vel.z)
    return potential * kinetic
end

local function simulateStep(moons)
    for i = 1, #moons do
        for j = i + 1, #moons do applyGravity(moons[i], moons[j]) end
    end
    for _, moon in ipairs(moons) do applyVelocity(moon) end
end

local function getAxisState(moons, axis)
    local state = {}
    for _, moon in ipairs(moons) do
        table.insert(state, moon.pos[axis])
        table.insert(state, moon.vel[axis])
    end
    return table.concat(state, ",")
end

local function findAxisCycle(moons, axis)
    local seen = {}
    local step = 0
    local copy = {}
    for i, moon in ipairs(moons) do
        copy[i] = {
            pos = { x = moon.pos.x, y = moon.pos.y, z = moon.pos.z },
            vel = { x = moon.vel.x, y = moon.vel.y, z = moon.vel.z }
        }
    end
    while true do
        local state = getAxisState(copy, axis)
        if seen[state] then return step end
        seen[state] = true
        for i = 1, #copy do
            for j = i + 1, #copy do
                if copy[i].pos[axis] < copy[j].pos[axis] then
                    copy[i].vel[axis] = copy[i].vel[axis] + 1
                    copy[j].vel[axis] = copy[j].vel[axis] - 1
                elseif copy[i].pos[axis] > copy[j].pos[axis] then
                    copy[i].vel[axis] = copy[i].vel[axis] - 1
                    copy[j].vel[axis] = copy[j].vel[axis] + 1
                end
            end
        end
        for _, moon in ipairs(copy) do moon.pos[axis] = moon.pos[axis] + moon.vel[axis] end
        step = step + 1
    end
end

local function gcd(a, b)
    while b ~= 0 do a, b = b, a % b end
    return a
end

local function lcm(a, b)
    return math.abs(a * b) / gcd(a, b)
end

local function solvePartOne()
    for _ = 1, 1000 do simulateStep(data) end
    local totalEnergy = 0
    for _, moon in ipairs(data) do totalEnergy = totalEnergy + calculateEnergy(moon) end
    return totalEnergy
end

local function solvePartTwo()
    local xCycle = findAxisCycle(data, "x")
    local yCycle = findAxisCycle(data, "y")
    local zCycle = findAxisCycle(data, "z")
    return string.format("%.0f", lcm(xCycle, lcm(yCycle, zCycle)))
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
