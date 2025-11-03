local utils = require("utils")

local program = {}
for value in utils.getInputData(25):gmatch("[^,]+") do
    table.insert(program, tonumber(value))
end

local function sendCommand(computer, command)
    for i = 1, #command do
        computer:addInput(string.byte(command, i))
    end
    computer:addInput(10)
end

local function runAndReadOutput(computer)
    computer:run()
    local output = {}
    while #computer.outputs > 0 do
        local char = computer:getOutput()
        if char then
            table.insert(output, string.char(char))
        end
    end
    return table.concat(output)
end

local function parseRoom(text)
    local room = {
        name = "",
        exits = {},
        items = {},
        description = ""
    }
    local state = "init"
    for line in text:gmatch("[^\n]+") do
        local name = line:match("^== ([^=]+) ==$")
        if name then
            room.name = name
            state = "description"
        elseif line:match("^Doors here lead:") then
            state = "exits"
        elseif line:match("^Items here:") then
            state = "items"
        elseif state == "description" and line ~= "" then
            room.description = room.description .. line
        elseif state == "exits" then
            local exit = line:match("^%- (.+)$")
            if exit then
                table.insert(room.exits, exit)
            end
        elseif state == "items" then
            local item = line:match("^%- (.+)$")
            if item then
                table.insert(room.items, item)
            end
        end
    end
    return room
end

local function opposite(dir)
    local opposites = {
        north = "south",
        south = "north",
        east = "west",
        west = "east"
    }
    return opposites[dir]
end

local function findPath(bot, target)
    local moves = {}
    local comeFrom = {}
    for _, dir in ipairs(bot.rooms[bot.current].exits) do
        table.insert(moves, { room = bot.current, dir = dir, cnt = 0 })
    end
    while #moves > 0 do
        local move = table.remove(moves, 1)
        local room, dir, cnt = move.room, move.dir, move.cnt
        if bot.connections[room] and bot.connections[room][dir] then
            local newRoom = bot.connections[room][dir]
            if not comeFrom[newRoom] or comeFrom[newRoom].cnt > cnt then
                comeFrom[newRoom] = { room = room, dir = dir, cnt = cnt }
                if bot.rooms[newRoom] then
                    for _, newDir in ipairs(bot.rooms[newRoom].exits) do
                        table.insert(moves, { room = newRoom, dir = newDir, cnt = cnt + 1 })
                    end
                end
            end
        end
    end
    local path = {}
    local room = target
    while room ~= bot.current do
        if not comeFrom[room] then
            return nil
        end
        table.insert(path, 1, comeFrom[room].dir)
        room = comeFrom[room].room
    end
    return path
end

local function solve()
    local computer = utils.intcode(program)
    computer:run()
    local bot = {
        connections = {},
        rooms = {},
        unknown = {},
        current = "",
        items = {},
        checkpointDir = nil
    }
    local dangerousItems = {
        ["infinite loop"] = true,
        ["molten lava"] = true,
        ["escape pod"] = true,
        ["photons"] = true,
        ["giant electromagnet"] = true
    }
    local output = runAndReadOutput(computer)
    local room = parseRoom(output)
    bot.current = room.name
    bot.rooms[room.name] = room
    bot.connections[room.name] = {}
    for _, dir in ipairs(room.exits) do
        table.insert(bot.unknown, { room = room.name, dir = dir })
    end
    while #bot.unknown > 0 do
        local next = table.remove(bot.unknown)
        local nextRoom, nextDir = next.room, next.dir
        if nextRoom == "Security Checkpoint" then
            if not bot.checkpointDir then
                bot.checkpointDir = nextDir
            end
            goto continue
        end
        if bot.connections[nextRoom] and bot.connections[nextRoom][nextDir] then
            goto continue
        end
        if nextRoom ~= bot.current then
            local path = findPath(bot, nextRoom)
            if path then
                for _, dir in ipairs(path) do
                    sendCommand(computer, dir)
                    output = runAndReadOutput(computer)
                end
                bot.current = parseRoom(output).name
            end
        end
        local prev = bot.current
        sendCommand(computer, nextDir)
        output = runAndReadOutput(computer)
        room = parseRoom(output)
        bot.current = room.name
        if not bot.rooms[room.name] then
            bot.rooms[room.name] = room
            bot.connections[room.name] = {}
            for _, dir in ipairs(room.exits) do
                table.insert(bot.unknown, { room = room.name, dir = dir })
            end
        end
        bot.connections[prev][nextDir] = room.name
        bot.connections[room.name][opposite(nextDir)] = prev
        for _, item in ipairs(room.items) do
            if not dangerousItems[item] then
                sendCommand(computer, "take " .. item)
                runAndReadOutput(computer)
                table.insert(bot.items, item)
            end
        end
        :: continue ::
    end
    local path = findPath(bot, "Security Checkpoint")
    if path then
        for _, dir in ipairs(path) do
            sendCommand(computer, dir)
            output = runAndReadOutput(computer)
        end
        bot.current = parseRoom(output).name
    end
    if not bot.checkpointDir then
        local currentRoom = bot.rooms[bot.current]
        for _, dir in ipairs(currentRoom.exits) do
            if not bot.connections[bot.current][dir] then
                bot.checkpointDir = dir
                break
            end
        end
    end
    if not bot.checkpointDir then
        return "ERROR: Could not find checkpoint direction!"
    end
    for mask = 0, (2 ^ #bot.items) - 1 do
        for _, item in ipairs(bot.items) do
            sendCommand(computer, "drop " .. item)
            runAndReadOutput(computer)
        end
        local heldItems = {}
        for i = 1, #bot.items do
            local bit_val = math.floor(mask / (2 ^ (i - 1))) % 2
            if bit_val == 1 then
                sendCommand(computer, "take " .. bot.items[i])
                runAndReadOutput(computer)
                table.insert(heldItems, bot.items[i])
            end
        end
        sendCommand(computer, bot.checkpointDir)
        output = runAndReadOutput(computer)
        if not output:match("lighter") and not output:match("heavier") then
            local password = output:match("(%d+)")
            if password then
                return password
            end
        end
    end
    return "No solution found"
end

print("Solution: " .. solve())