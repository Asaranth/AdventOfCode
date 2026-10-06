--- Day 23: Category Six
---
--- Simulates a 50-node asynchronous packet routing network and NAT traffic controller with Intcode.

local utils = require("utils")

local program = {}
for value in utils.getInputData(23):gmatch("[^,]+") do
    table.insert(program, tonumber(value))
end

--- Initialises the 50-node Intcode network, assigning network addresses 0 through 49 to each node.
---
--- @return table[] computers Array of 50 Intcode computer instances.
--- @return table<number, table[]> queues Array of packet queues for each node address.
local function initializeNetwork()
    local computers = {}
    for i = 1, 50 do
        computers[i] = utils.intcode(program)
        computers[i]:addInput(i - 1)
    end
    local queues = {}
    for i = 1, 50 do
        queues[i] = {}
    end
    return computers, queues
end

--- Delivers the next pending packet to a computer's input queue, or inputs -1 when no packets are queued.
---
--- @param computer IntcodeComputer The node computer instance.
--- @param queue table[] The FIFO queue of incoming packets for this node.
--- @return boolean True if a packet was delivered, false if -1 was sent due to an empty queue.
local function processComputer(computer, queue)
    if #queue > 0 then
        local packet = table.remove(queue, 1)
        computer:addInput(packet.x)
        computer:addInput(packet.y)
        return true
    else
        computer:addInput(-1)
        return false
    end
end

--- Extracts all complete 3-value packet tuples [destination, X, Y] emitted by a computer.
---
--- @param computer IntcodeComputer The node computer instance.
--- @return { dest: number, x: number, y: number }[] List of parsed outgoing packets.
local function collectOutputPackets(computer)
    local packets = {}
    while #computer.outputs >= 3 do
        local dest = computer:getOutput()
        local x = computer:getOutput()
        local y = computer:getOutput()
        table.insert(packets, { dest = dest, x = x, y = y })
    end
    return packets
end

--- Simulates network packet routing and returns the Y coordinate of the first packet sent to address 255.
---
--- @return number The Y coordinate sent to address 255.
local function solvePartOne()
    local computers, queues = initializeNetwork()
    while true do
        for addr = 1, 50 do
            processComputer(computers[addr], queues[addr])
            computers[addr]:run()
            local packets = collectOutputPackets(computers[addr])
            for _, packet in ipairs(packets) do
                if packet.dest == 255 then
                    return packet.y
                end
                if packet.dest >= 0 and packet.dest <= 49 then
                    table.insert(queues[packet.dest + 1], { x = packet.x, y = packet.y })
                end
            end
        end
    end
    return 0
end

--- Simulates the network with a NAT device and returns the first Y coordinate delivered twice consecutively to address 0.
---
--- @return number The repeated Y value delivered by the NAT.
local function solvePartTwo()
    local computers, queues = initializeNetwork()
    local nat, lastNatY
    local idleCount = 0
    while true do
        local anyActivity = false
        for addr = 1, 50 do
            local hadInput = processComputer(computers[addr], queues[addr])
            computers[addr]:run()
            local packets = collectOutputPackets(computers[addr])
            if hadInput or #packets > 0 then
                anyActivity = true
            end
            for _, packet in ipairs(packets) do
                if packet.dest == 255 then
                    nat = { x = packet.x, y = packet.y }
                elseif packet.dest >= 0 and packet.dest <= 49 then
                    table.insert(queues[packet.dest + 1], { x = packet.x, y = packet.y })
                end
            end
        end
        if not anyActivity then
            idleCount = idleCount + 1
            if idleCount > 1 and nat then
                table.insert(queues[1], { x = nat.x, y = nat.y })
                if lastNatY == nat.y then
                    return nat.y
                end
                lastNatY = nat.y
                idleCount = 0
            end
        else
            idleCount = 0
        end
    end
    return 0
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())