--- Day 12: The N-Body Problem
---
--- Simulates 3D gravitational orbital mechanics and calculates axis periodicity via LCM.

local utils = require("utils")

local data = {}
for line in utils.getInputData(12):gmatch("[^\r\n]+") do
    local x, y, z = line:match("<x=(-?%d+), y=(-?%d+), z=(-?%d+)>")
    table.insert(data, {
        pos = { x = tonumber(x), y = tonumber(y), z = tonumber(z) },
        vel = { x = 0, y = 0, z = 0 }
    })
end

--- Applies gravitational attraction between two moons, updating their velocities along each axis.
---
--- @param m1 table The first moon with pos and vel tables.
--- @param m2 table The second moon with pos and vel tables.
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

--- Applies velocity to update a moon's 3D coordinates.
---
--- @param moon table Moon state containing pos and vel tables.
local function applyVelocity(moon)
    moon.pos.x = moon.pos.x + moon.vel.x
    moon.pos.y = moon.pos.y + moon.vel.y
    moon.pos.z = moon.pos.z + moon.vel.z
end

--- Calculates total mechanical energy of a moon as the product of its potential and kinetic energies.
---
--- @param moon table Moon state containing pos and vel tables.
--- @return number Total mechanical energy.
local function calculateEnergy(moon)
    local potential = math.abs(moon.pos.x) + math.abs(moon.pos.y) + math.abs(moon.pos.z)
    local kinetic = math.abs(moon.vel.x) + math.abs(moon.vel.y) + math.abs(moon.vel.z)
    return potential * kinetic
end

--- Advances the simulation by a single time step across all moons.
---
--- @param moons table[] Array of moon objects.
local function simulateStep(moons)
    for i = 1, #moons do
        for j = i + 1, #moons do applyGravity(moons[i], moons[j]) end
    end
    for _, moon in ipairs(moons) do applyVelocity(moon) end
end

--- Serialises 1D position and velocity state across all moons along a specific coordinate axis.
---
--- @param moons table[] Array of moon objects.
--- @param axis string The coordinate axis ("x", "y", or "z").
--- @return string Comma-separated string encoding the 1D state.
local function getAxisState(moons, axis)
    local state = {}
    for _, moon in ipairs(moons) do
        table.insert(state, moon.pos[axis])
        table.insert(state, moon.vel[axis])
    end
    return table.concat(state, ",")
end

--- Determines the step cycle length for a single independent coordinate axis.
---
--- @param moons table[] Array of initial moon objects.
--- @param axis string The coordinate axis ("x", "y", or "z").
--- @return number The number of steps before the axis state repeats.
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

--- Calculates the greatest common divisor of two integers.
---
--- @param a number First integer.
--- @param b number Second integer.
--- @return number The greatest common divisor.
local function gcd(a, b)
    while b ~= 0 do a, b = b, a % b end
    return a
end

--- Calculates the lowest common multiple of two integers.
---
--- @param a number First integer.
--- @param b number Second integer.
--- @return number The lowest common multiple.
local function lcm(a, b)
    return math.abs(a * b) / gcd(a, b)
end

--- Simulates 1,000 steps and computes the total energy across all moons for Part One.
---
--- @return number The sum of total energy.
local function solvePartOne()
    for _ = 1, 1000 do simulateStep(data) end
    local totalEnergy = 0
    for _, moon in ipairs(data) do totalEnergy = totalEnergy + calculateEnergy(moon) end
    return totalEnergy
end

--- Calculates the total steps required for all moons to return to their initial state for Part Two.
---
--- @return string The combined cycle length across all three axes.
local function solvePartTwo()
    local xCycle = findAxisCycle(data, "x")
    local yCycle = findAxisCycle(data, "y")
    local zCycle = findAxisCycle(data, "z")
    return string.format("%.0f", lcm(xCycle, lcm(yCycle, zCycle)))
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
