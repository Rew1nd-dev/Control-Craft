-- Demo: periodically write a sine wave into the input port "angle"
-- of a named component through Bus.propagate(...), then render the
-- current written value on the computer screen.

local TARGET_COMPONENT = "controller_1"
local TARGET_PORT = "angle"

local INITIALIZED = false
local SERVER_TICK = 0

local AMPLITUDE = 45.0
local PERIOD_TICKS = 80.0
local OFFSET = 0.0

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

local function computeWave(tick)
    local phase = (tick / PERIOD_TICKS) * math.pi * 2.0
    return OFFSET + math.sin(phase) * AMPLITUDE
end

function onServerTick()
    SERVER_TICK = SERVER_TICK + 1

    local angle = computeWave(SERVER_TICK)
    Bus.propagate(TARGET_COMPONENT, TARGET_PORT, angle)

    Network.set("bus_write_target_component", TARGET_COMPONENT)
    Network.set("bus_write_target_port", TARGET_PORT)
    Network.set("bus_write_angle_value", angle)
    Network.set("bus_write_server_tick", SERVER_TICK)
end

function onClientTick()
    initClient()

    local w, h = getCanvasSize()
    local angle = Network.peek("bus_write_angle_value") or 0.0
    local serverTick = Network.peek("bus_write_server_tick") or 0
    local componentName = Network.peek("bus_write_target_component") or TARGET_COMPONENT
    local portName = Network.peek("bus_write_target_port") or TARGET_PORT

    local cx = math.floor(w * 0.5)
    local cy = math.floor(h * 0.5)
    local meterW = math.max(20, math.floor(w * 0.7))
    local meterX = math.floor((w - meterW) * 0.5)
    local meterY = cy + 28

    local normalized = math.max(-1.0, math.min(1.0, angle / math.max(1.0, AMPLITUDE)))
    local indicatorX = meterX + math.floor((normalized * 0.5 + 0.5) * meterW)

    render.clear()

    render.setColor(18, 32, 52, 255)
    render.setOpacity(0.58)
    render.drawRect(0, 0, w, h)
    render.setOpacity(1.0)

    render.setColor(70, 110, 170, 200)
    render.drawRect(meterX, meterY, meterW, 6)
    render.drawRect(cx, meterY - 6, 1, 18)

    render.pushLayer()
    render.setColor(100, 240, 160, 255)
    render.drawRect(indicatorX - 2, meterY - 5, 5, 16)

    render.setColor(255, 255, 255, 245)
    render.drawText("BUS ANGLE WRITE DEMO", 8, 8, 0.82)
    render.drawText("component: " .. tostring(componentName), 8, 24, 0.7)
    render.drawText("port: " .. tostring(portName), 8, 36, 0.7)
    render.drawText(string.format("write angle: %.3f", angle), 8, 52, 0.9)
    render.drawText(string.format("amplitude: %.1f  period: %.1f", AMPLITUDE, PERIOD_TICKS), 8, 64, 0.65)
    render.drawText("serverTick: " .. tostring(serverTick), 8, h - 12, 0.7)
    render.drawText(string.format("%.0f", -AMPLITUDE), meterX - 18, meterY + 10, 0.55)
    render.drawText("0", cx - 3, meterY + 10, 0.55)
    render.drawText(string.format("%.0f", AMPLITUDE), meterX + meterW - 6, meterY + 10, 0.55)

    render.submit()
end
