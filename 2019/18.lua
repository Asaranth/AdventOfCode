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
        if heap[parentIndex][1] <= heap[index][1] then break end
        heap[parentIndex], heap[index] = heap[index], heap[parentIndex]
        index = parentIndex
    end
end

local function heapPop(heap)
    if #heap == 0 then return nil end
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
        if smallest == index then break end
        heap[index], heap[smallest] = heap[smallest], heap[index]
        index = smallest
    end
    return min
end

local function solvePartOne()
    local heap = {}
    local visited = {}
    local initialKeys = 0
    local allKeysMask = (1 << totalKeys) - 1
    heapPush(heap, { 0, startX, startY, initialKeys })

    while #heap > 0 do
        local steps, x, y, keys = table.unpack(heapPop(heap))
        local key = (x - 1) * ROWS + y * (1 << totalKeys) + keys

        if visited[key] then goto continue end
        visited[key] = true

        local tile = data[y]:sub(x, x)
        if tile:match("%l") then
            local bit = 1 << (tile:byte() - string.byte('a'))
            keys = keys | bit
            if keys == allKeysMask then
                return steps
            end
        end
        if tile:match("%u") then
            local bit = 1 << (tile:byte() - string.byte('A'))
            if (keys & bit) == 0 then
                goto continue
            end
        end
        for _, dir in ipairs(DIRECTIONS) do
            local nx, ny = x + dir[1], y + dir[2]
            if inBounds(nx, ny) then
                local nextTile = data[ny]:sub(nx, nx)
                if nextTile ~= '#' then
                    heapPush(heap, { steps + 1, nx, ny, keys })
                end
            end
        end
        ::continue::
    end
end


local function solvePartTwo()
    return 0
end

print("Part One: " .. solvePartOne())
print("Part Two: " .. solvePartTwo())
