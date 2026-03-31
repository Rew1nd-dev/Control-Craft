-- Demo: read a named Cimulink component output port "angle" on server,
-- sync it to client with Network, then render it on the computer screen.

local TARGET_COMPONENT = "controller_1"
local TARGET_PORT = "angle"

local INITIALIZED = false
local SERVER_TICK = 0

local function getCanvasSize()
    local w = (render.getWidth and render.getWidth()) or render.width or 320
    local h = (render.getHeight and render.getHeight()) or render.height or 180
    return math.max(1, w), math.max(1, h)
end

local function initClient()
    if INITIALIZED then
        return
    end

    render.setResolution(320, 180)
    render.setSurfaceSize(1.8, 1.0)
    render.setOffset(0.0, 1.0, -0.5)
    INITIALIZED = true
end

function onServerTick()
    SERVER_TICK = SERVER_TICK + 1

    local angle = Bus.retrieve(TARGET_COMPONENT, TARGET_PORT)
    Network.set("bus_target_component", TARGET_COMPONENT)
    Network.set("bus_target_port", TARGET_PORT)
    Network.set("bus_angle_value", angle)
    Network.set("bus_server_tick", SERVER_TICK)
end

function onClientTick()
    initClient()

    local w, h = getCanvasSize()
    local angle = Network.peek("bus_angle_value") or 0.0
    local serverTick = Network.peek("bus_server_tick") or 0
    local componentName = Network.peek("bus_target_component") or TARGET_COMPONENT
    local portName = Network.peek("bus_target_port") or TARGET_PORT

    local cx = math.floor(w * 0.5)
    local cy = math.floor(h * 0.5)
    local meterW = math.max(20, math.floor(w * 0.7))
    local meterX = math.floor((w - meterW) * 0.5)
    local meterY = cy + 28

    local normalized = math.max(-1.0, math.min(1.0, angle / 180.0))
    local indicatorX = meterX + math.floor((normalized * 0.5 + 0.5) * meterW)

    render.clear()

    render.setColor(14, 26, 44, 255)
    render.setOpacity(0.56)
    render.drawRect(0, 0, w, h)
    render.setOpacity(1.0)

    render.setColor(70, 110, 170, 200)
    render.drawRect(meterX, meterY, meterW, 6)
    render.drawRect(cx, meterY - 6, 1, 18)

    render.pushLayer()
    render.setColor(255, 220, 90, 255)
    render.drawRect(indicatorX - 2, meterY - 5, 5, 16)

    render.setColor(255, 255, 255, 245)
    render.drawText("BUS ANGLE DEMO", 8, 8, 0.85)
    render.drawText("component: " .. tostring(componentName), 8, 24, 0.7)
    render.drawText("port: " .. tostring(portName), 8, 36, 0.7)
    render.drawText(string.format("angle: %.3f", angle), 8, 52, 0.9)
    render.drawText("serverTick: " .. tostring(serverTick), 8, h - 12, 0.7)
    render.drawText("-180", meterX - 12, meterY + 10, 0.55)
    render.drawText("0", cx - 3, meterY + 10, 0.55)
    render.drawText("180", meterX + meterW - 2, meterY + 10, 0.55)

    render.submit()
end
