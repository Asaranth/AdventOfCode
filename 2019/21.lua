local utils = require("utils")

local program = {}
for value in utils.getInputData(21):gmatch("[^,]+") do
    table.insert(program, tonumber(value))
end

local function solve(instructions)
    local computer = utils.intcode(program)
    for _, instruction in ipairs(instructions) do
        for i = 1, #instruction do
            computer:addInput(string.byte(instruction, i))
        end
        computer:addInput(10)
    end
    computer:run()
    local output = {}
    while not computer:isHalted() do
        computer:run()
    end
    local result
    while true do
        local out = computer:getOutput()
        if not out then
            break
        end
        table.insert(output, out)
        result = out
    end
    return result
end

print("Part One: " .. solve({ "NOT A J", "NOT B T", "OR T J", "NOT C T", "OR T J", "AND D J", "WALK" }))
print("Part Two: " .. solve({ "NOT A J", "NOT B T", "OR T J", "NOT C T", "OR T J", "AND D J", "NOT E T", "NOT T T", "OR H T", "AND T J", "RUN" }))