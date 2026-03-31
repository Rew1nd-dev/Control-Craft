local INITIALIZED = false

local function clamp(v, lo, hi)
    if v < lo then
        return lo
    end
    if v > hi then
        return hi
    end
    return v
end

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

local function drawCross(x, y, size)
    local half = math.floor(size * 0.5)
    render.drawRect(math.floor(x) - half, math.floor(y), size, 1)
    render.drawRect(math.floor(x), math.floor(y) - half, 1, size)
end

function onClientTick()
    initClient()

    local w, h = getCanvasSize()
    local cx = math.floor(w * 0.5)
    local cy = math.floor(h * 0.5)
    local watch = Player.getPlayerWatch(0.0)

    render.clear()

    render.setColor(12, 28, 48, 255)
    render.setOpacity(0.48)
    render.drawRect(0, 0, w, h)
    render.setOpacity(1.0)

    render.setColor(90, 140, 190, 200)
    render.drawRect(0, 0, w, 2)
    render.drawRect(0, h - 2, w, 2)
    render.drawRect(0, 0, 2, h)
    render.drawRect(w - 2, 0, 2, h)

    render.setColor(90, 180, 255, 120)
    drawCross(cx, cy, 11)

    render.pushLayer()
    render.setColor(255, 255, 255, 245)
    render.drawText("PLAYER WATCH DEMO", 8, 8, 0.8)

    if watch then
        local px = watch.x
        local py = watch.y
        local onScreen = watch.onScreen == true

        if onScreen then
            render.setColor(255, 225, 90, 255)
            drawCross(px, py, 13)
            render.drawRect(math.floor(px) - 2, math.floor(py) - 2, 5, 5)
        else
            local clampedX = clamp(px, 3, w - 4)
            local clampedY = clamp(py, 3, h - 4)
            render.setColor(255, 100, 100, 255)
            drawCross(clampedX, clampedY, 11)
            render.drawRect(math.floor(clampedX) - 2, math.floor(clampedY) - 2, 5, 5)
        end

        render.setColor(255, 255, 255, 235)
        render.drawText(string.format("pixel: %.1f, %.1f", px, py), 8, h - 28, 0.7)
        render.drawText("onScreen: " .. tostring(onScreen), 8, h - 18, 0.7)
        render.drawText(string.format("distance: %.2f", watch.distance or 0.0), 8, h - 8, 0.7)
    else
        render.setColor(255, 170, 170, 245)
        render.drawText("watch: nil", 8, h - 18, 0.75)
        render.drawText("Need mounted player and a forward screen hit", 8, h - 8, 0.6)
    end

    render.submit()
end
