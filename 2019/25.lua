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

local computer = utils.intcode(program)
local output = runAndReadOutput(computer)
print(output)

while not computer.halted do
    io.write("> ")
    local command = io.read()
    if command and command ~= "" then
        sendCommand(computer, command)
        output = runAndReadOutput(computer)
        print(output)
    end
end