-- API demo for Control Craft computer
-- Covers:
-- 1) render.setResolution / setSurfaceSize / setOffset
-- 2) Block front/left/up vectors
-- 3) Player yaw/pitch + ShipMountedToData
-- 4) client->server Network.send and server echo

local SERVER_TICK = 0

local CLIENT_TICK = 0
local LAST_ECHO = "none"
local ACK_COUNT = 0
local INITIALIZED = false
local TEST_OVERSIZE_ONCE = false
local TEST_OVERSIZE_SENT = false

local function n(v, fallback)
    if type(v) == "number" then
        return v
    end
    return fallback
end

local function vecToString(v)
    if type(v) ~= "table" then
        return "nil"
    end
    return string.format("(%.2f, %.2f, %.2f)", n(v.x, 0.0), n(v.y, 0.0), n(v.z, 0.0))
end

local function shorten(s, maxLen)
    if type(s) ~= "string" then
        s = tostring(s)
    end
    if #s <= maxLen then
        return s
    end
    return string.sub(s, 1, maxLen - 3) .. "..."
end

function onServerTick()
    SERVER_TICK = SERVER_TICK + 1

    if Network.isDirty("client_ping") then
        local ping = Network.retrieve("client_ping")
        Network.set("server_echo", "echo@" .. tostring(SERVER_TICK) .. " " .. tostring(ping))
        Network.set("server_tick", SERVER_TICK)
    elseif SERVER_TICK % 20 == 0 then
        Network.set("server_tick", SERVER_TICK)
    end

    -- If this ever becomes true, the 1024-byte reject is likely broken.
    if Network.isDirty("oversize_probe") then
        Network.retrieve("oversize_probe")
        Network.set("oversize_accepted", true)
    end
end

local function initClientOnce()
    if INITIALIZED then
        return
    end

    render.setResolution(320, 180)
    render.setSurfaceSize(1.8, 1.0)
    render.setOffset(0.0, 1.0, -0.5)
    INITIALIZED = true
end

function onClientTick()
    initClientOnce()
    CLIENT_TICK = CLIENT_TICK + 1

    local w = (render.getWidth and render.getWidth()) or render.width or 320
    local h = (render.getHeight and render.getHeight()) or render.height or 180

    local yaw = (Player and Player.yaw and Player.yaw(0.0)) or 0.0
    local pitch = (Player and Player.pitch and Player.pitch(0.0)) or 0.0
    local mounted = (Player and Player.getShipMountedToData and Player.getShipMountedToData(0.0)) or nil

    local front = (Block and Block.front and Block.front()) or nil
    local left = (Block and Block.left and Block.left()) or nil
    local up = (Block and Block.up and Block.up()) or nil

    if CLIENT_TICK % 20 == 1 then
        local msg = string.format("cTick=%d yaw=%.1f pitch=%.1f", CLIENT_TICK, yaw, pitch)
        Network.send("client_ping", msg)
        Network.send("client_pitch", pitch)
    end

    if TEST_OVERSIZE_ONCE and not TEST_OVERSIZE_SENT then
        Network.send("oversize_probe", string.rep("X", 1100))
        TEST_OVERSIZE_SENT = true
    end

    if Network.isDirty("server_echo") then
        LAST_ECHO = tostring(Network.retrieve("server_echo"))
        ACK_COUNT = ACK_COUNT + 1
    end

    local serverTick = n(Network.peek("server_tick"), 0)
    local oversizeAccepted = Network.peek("oversize_accepted")

    local fx = n(front and front.x, 0.0)
    local barW = math.max(0, math.min(w - 20, math.floor((fx * 0.5 + 0.5) * (w - 20))))

    render.clear()

    -- Transparent blue background
    render.setColor(20, 90, 190, 255)
    render.setOpacity(0.45)
    render.drawRect(0, 0, w, h)
    render.setOpacity(1.0)

    -- Simple vector indicator bar by front.x
    render.setColor(60, 190, 255, 200)
    render.drawRect(10, h - 16, w - 20, 6)
    render.setColor(255, 210, 90, 255)
    render.drawRect(10, h - 16, barW, 6)

    render.pushLayer()
    render.setColor(255, 255, 255, 255)
    render.drawText("ControlCraft API Demo", 8, 8, 0.85)
    render.drawText(
        string.format("canvas=%dx%d surface=%.2fx%.2f", w, h, render.getSurfaceWidth(), render.getSurfaceHeight()),
        8,
        22,
        0.65
    )
    render.drawText(string.format("yaw=%.1f pitch=%.1f", yaw, pitch), 8, 34, 0.65)
    render.drawText("front " .. vecToString(front), 8, 46, 0.65)
    render.drawText("left  " .. vecToString(left), 8, 58, 0.65)
    render.drawText("up    " .. vecToString(up), 8, 70, 0.65)

    if type(mounted) == "table" and mounted.mountPosInShip then
        render.drawText("mounted shipId=" .. tostring(mounted.shipMountedToId), 8, 86, 0.62)
        render.drawText("mountPos " .. vecToString(mounted.mountPosInShip), 8, 97, 0.62)
    else
        render.drawText("mounted: nil", 8, 86, 0.62)
    end

    render.drawText("serverTick=" .. tostring(serverTick) .. " ack=" .. tostring(ACK_COUNT), 8, h - 30, 0.68)
    render.drawText("echo: " .. shorten(LAST_ECHO, 52), 8, h - 20, 0.60)
    render.drawText("oversizeAccepted=" .. tostring(oversizeAccepted), 8, h - 10, 0.60)

    render.submit()
end
