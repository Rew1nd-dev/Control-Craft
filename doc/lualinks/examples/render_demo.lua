-- Sample script for Control Craft computer rendering pipeline.
-- Server: pushes simple state via Network.set(...)
-- Client: reads state and draws with render.*

local tick = 0
local lastTouchX = nil
local lastTouchY = nil

local function safeNumber(v, fallback)
    if type(v) == "number" then
        return v
    end
    return fallback
end

function onServerTick()
    tick = tick + 1

    local pulse = (math.sin(tick * 0.08) + 1.0) * 0.5
    Network.set("tick", tick)
    Network.set("pulse", pulse)
end

function onPlayerEvent(event)
    if type(event) ~= "table" then
        return
    end

    if event.type == "touch" then
        lastTouchX = safeNumber(event.x, 0)
        lastTouchY = safeNumber(event.y, 0)
        Network.set("touchX", lastTouchX)
        Network.set("touchY", lastTouchY)
    end
end

function onClientTick()
    local w = (render.getWidth and render.getWidth()) or render.width or 256
    local h = (render.getHeight and render.getHeight()) or render.height or 256

    local t = safeNumber(Network.peek("tick"), 0)
    local pulse = safeNumber(Network.peek("pulse"), 0)
    local touchX = Network.peek("touchX")
    local touchY = Network.peek("touchY")

    render.clear()

    -- Background
    render.setColor(0, 112, 255, 255) -- Blue-2 style
    render.setOpacity(0.65)
    render.drawRect(0, 0, w, h)
    render.setOpacity(1.0)

    -- Animated bar
    local barMax = math.max(1, w - 16)
    local barW = math.floor(barMax * pulse)
    render.setColor(70, 190, 255, 255)
    render.drawRect(8, h - 14, barW, 8)

    -- Animated cursor
    local x = 8 + math.floor((w - 16) * pulse)
    local y = 50 + math.floor(math.sin(t * 0.2) * 16)
    render.setColor(255, 110, 90, 255)
    render.drawRect(x - 2, y - 2, 4, 4)

    -- Move subsequent commands to a higher layer.
    render.pushLayer()

    -- Text
    render.setColor(255, 255, 255, 255)
    render.drawText("Control Craft Render Demo", 8, 8, 1.0)
    render.drawText("size: " .. tostring(w) .. "x" .. tostring(h), 8, 22, 0.8)
    render.drawText("tick: " .. tostring(t), 8, 34, 0.8)

    if type(touchX) == "number" and type(touchY) == "number" then
        render.drawText("touch: (" .. tostring(math.floor(touchX)) .. ", " .. tostring(math.floor(touchY)) .. ")", 8, 46, 0.8)
    else
        render.drawText("touch: (none)", 8, 46, 0.8)
    end

    render.submit()
end
