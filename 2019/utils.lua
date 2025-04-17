local https = require("ssl.https")
local ltn12 = require("ltn12")

local function loadEnv()
    local envFile = io.open("../.env", "r")
    if not envFile then
        return {}
    end
    local envVars = {}
    for line in envFile:lines() do
        local key, value = tostring(line):match("^([^=]+)%s*=%s*(.+)$")
        if key and value then
            envVars[key] = value
        end
    end
    envFile:close()
    return envVars
end

local env = loadEnv()
local sessionCookie = env["AOC_SESSION_COOKIE"]

if not sessionCookie then
    error("AOC_SESSION_COOKIE not found in environment variables")
end

local function getInputData(day)
    local cacheFile = string.format("data/%02d.txt", day)
    local file = io.open(cacheFile, "r")
    if file then
        local data = file:read("*all")
        file:close()
        return data
    end
    local url = string.format("https://adventofcode.com/2019/day/%d/input", day)
    local response = {}
    local _, status = https.request {
        url = url,
        headers = { ["Cookie"] = "session=" .. sessionCookie },
        sink = ltn12.sink.table(response)
    } or {}, nil
    if status ~= 200 then
        error("Failed to fetch data. HTTP status: " .. tostring(status))
    end
    local data = table.concat(response)
    os.execute("mkdir -p data")
    local outputFile = io.open(cacheFile, "w")
    if outputFile == nil then
        error("Output File not found.")
    end
    outputFile:write(data)
    outputFile:close()
    return data
end

return { getInputData = getInputData }