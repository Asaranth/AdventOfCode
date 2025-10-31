local utils = require("utils")

local data = {}
for line in utils.getInputData(18):gmatch("[^\n]+") do
    table.insert(data, line)
end

local ROWS, COLS = #data, #data[1]
local DIRECTIONS = { { 0, 1 }, { 1, 0 }, { 0, -1 }, { -1, 0 } }
local totalKeys = 0
local startX, startY = 0, 0

for y = 1, ROWS do
    for x = 1, COLS do
        local c = data[y]:sub(x, x)
        if c == '@' then
            startX, startY = x, y
        elseif c:match("%l") then
            totalKeys = totalKeys + 1
        end
    end
end

local function inBounds(x, y)
    return x >= 1 and x <= COLS and y >= 1 and y <= ROWS
end

local function heapPush(heap, value)
    table.insert(heap, value)
    local index = #heap
    while index > 1 do
        local parentIndex = math.floor(index / 2)
        if heap[parentIndex][1] <= heap[index][1] then
            break
        end
        heap[parentIndex], heap[index] = heap[index], heap[parentIndex]
        index = parentIndex
    end
end

local function heapPop(heap)
    if #heap == 0 then
        return nil
    end
    local min = heap[1]
    heap[1] = heap[#heap]
    table.remove(heap)
    local index = 1
    while true do
        local leftChild = 2 * index
        local rightChild = 2 * index + 1
        local smallest = index
        if leftChild <= #heap and heap[leftChild][1] < heap[smallest][1] then
            smallest = leftChild
        end
        if rightChild <= #heap and heap[rightChild][1] < heap[smallest][1] then
            smallest = rightChild
        end
        if smallest == index then
            break
        end
        heap[index], heap[smallest] = heap[smallest], heap[index]
        index = smallest
    end
    return min
end

local function getTileBit(tile)
    if tile:match("%l") then
        return 1 << (tile:byte() - string.byte('a'))
    elseif tile:match("%u") then
        return 1 << (tile:byte() - string.byte('A'))
    end
    return nil
end

local function solvePartOne()
    local heap = {}
    local visited = {}
    local allKeysMask = (1 << totalKeys) - 1
    heapPush(heap, { 0, startX, startY, 0 })
    while #heap > 0 do
        local steps, x, y, keys = table.unpack(heapPop(heap))
        local key = (x - 1) * ROWS + y * (1 << totalKeys) + keys
        if visited[key] then
            goto continue
        end
        visited[key] = true
        local tile = data[y]:sub(x, x)
        if tile:match("%l") then
            keys = keys | getTileBit(tile)
            if keys == allKeysMask then
                return steps
            end
        end
        if tile:match("%u") and (keys & getTileBit(tile)) == 0 then
            goto continue
        end
        for _, dir in ipairs(DIRECTIONS) do
            local nx, ny = x + dir[1], y + dir[2]
            if inBounds(nx, ny) and data[ny]:sub(nx, nx) ~= '#' then
                heapPush(heap, { steps + 1, nx, ny, keys })
            end
        end
        :: continue ::
    end
end

local function solvePartTwo()
    local mapCopy = {}
    for i = 1, #data do
        mapCopy[i] = data[i]
    end

    local centerX, centerY = startX, startY
    mapCopy[centerY - 1] = mapCopy[centerY - 1]:sub(1, centerX - 2) .. "@#@" .. mapCopy[centerY - 1]:sub(centerX + 2)
    mapCopy[centerY] = mapCopy[centerY]:sub(1, centerX - 2) .. "###" .. mapCopy[centerY]:sub(centerX + 2)
    mapCopy[centerY + 1] = mapCopy[centerY + 1]:sub(1, centerX - 2) .. "@#@" .. mapCopy[centerY + 1]:sub(centerX + 2)
    local starts = { { centerX - 1, centerY - 1 }, { centerX + 1, centerY - 1 }, { centerX - 1, centerY + 1 }, { centerX + 1, centerY + 1 } }

    local function findReachableKeys(sx, sy, currentKeys)
        local queue = { { sx, sy, 0 } }
        local visited = {}
        local reachable = {}
        local queueStart, queueEnd = 1, 1
        visited[sy * 1000 + sx] = true
        while queueStart <= queueEnd do
            local x, y, dist = table.unpack(queue[queueStart])
            queueStart = queueStart + 1
            local tile = mapCopy[y]:sub(x, x)
            if tile:match("%l") then
                local bit = getTileBit(tile)
                if (currentKeys & bit) == 0 then
                    reachable[tile] = { dist, x, y }
                    goto nextInQueue
                end
            end
            if tile:match("%u") and (currentKeys & getTileBit(tile)) == 0 then
                goto nextInQueue
            end
            for _, dir in ipairs(DIRECTIONS) do
                local nx, ny = x + dir[1], y + dir[2]
                local visitKey = ny * 1000 + nx
                if inBounds(nx, ny) and not visited[visitKey] and mapCopy[ny]:sub(nx, nx) ~= '#' then
                    visited[visitKey] = true
                    queueEnd = queueEnd + 1
                    queue[queueEnd] = { nx, ny, dist + 1 }
                end
            end
            :: nextInQueue ::
        end
        return reachable
    end

    local heap = {}
    local visited = {}
    local allKeysMask = (1 << totalKeys) - 1
    heapPush(heap, { 0, starts[1][1], starts[1][2], starts[2][1], starts[2][2], starts[3][1], starts[3][2], starts[4][1], starts[4][2], 0 })
    while #heap > 0 do
        local state = heapPop(heap)
        local steps = state[1]
        local pos = { { state[2], state[3] }, { state[4], state[5] }, { state[6], state[7] }, { state[8], state[9] } }
        local keys = state[10]
        if keys == allKeysMask then
            return steps
        end
        local stateKey = string.format("%d_%d_%d_%d_%d_%d_%d_%d_%d", pos[1][1], pos[1][2], pos[2][1], pos[2][2], pos[3][1], pos[3][2], pos[4][1], pos[4][2], keys)
        if visited[stateKey] then
            goto continue
        end
        visited[stateKey] = true
        for robotIdx = 1, 4 do
            local x, y = pos[robotIdx][1], pos[robotIdx][2]
            local reachable = findReachableKeys(x, y, keys)
            for keyChar, info in pairs(reachable) do
                local dist, keyX, keyY = info[1], info[2], info[3]
                local newKeys = keys | getTileBit(keyChar)
                local newState = { steps + dist }
                for i = 1, 4 do
                    if i == robotIdx then
                        table.insert(newState, keyX)
                        table.insert(newState, keyY)
                    else
                        table.insert(newState, pos[i][1])
                        table.insert(newState, pos[i][2])
                    end
                end
                table.insert(newState, newKeys)
                heapPush(heap, newState)
            end
        end
        :: continue ::
    end
    return -1
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())