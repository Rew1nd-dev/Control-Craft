-- Attitude HUD demo for Control Craft computer
-- Uses screen-plane/world-horizontal intersection for the horizon line.

local RAD2DEG = 57.29577951308232

local function clamp(v, lo, hi)
    if v < lo then
        return lo
    end
    if v > hi then
        return hi
    end
    return v
end

local function toNumber(v, fallback)
    if type(v) == "number" then
        return v
    end
    return fallback
end

local function getCanvasSize()
    local w = (render.getWidth and render.getWidth()) or render.width or 256
    local h = (render.getHeight and render.getHeight()) or render.height or 256
    return math.max(1, w), math.max(1, h)
end

local function vec3(x, y, z)
    if type(Vector3d) == "table" and Vector3d.new then
        return Vector3d:new(x, y, z)
    end
    return { x = x or 0.0, y = y or 0.0, z = z or 0.0 }
end

local function vdot(a, b)
    if type(a) == "table" and a.dot then
        return a:dot(b)
    end
    return toNumber(a.x, 0.0) * toNumber(b.x, 0.0)
        + toNumber(a.y, 0.0) * toNumber(b.y, 0.0)
        + toNumber(a.z, 0.0) * toNumber(b.z, 0.0)
end

local function vcross(a, b)
    if type(a) == "table" and a.cross then
        return a:cross(b)
    end
    return vec3(
        toNumber(a.y, 0.0) * toNumber(b.z, 0.0) - toNumber(a.z, 0.0) * toNumber(b.y, 0.0),
        toNumber(a.z, 0.0) * toNumber(b.x, 0.0) - toNumber(a.x, 0.0) * toNumber(b.z, 0.0),
        toNumber(a.x, 0.0) * toNumber(b.y, 0.0) - toNumber(a.y, 0.0) * toNumber(b.x, 0.0)
    )
end

local function vneg(a)
    if type(a) == "table" and a.negate then
        return a:negate()
    end
    return vec3(-toNumber(a.x, 0.0), -toNumber(a.y, 0.0), -toNumber(a.z, 0.0))
end

local function vlen2(a)
    if type(a) == "table" and a.lengthSquared then
        return a:lengthSquared()
    end
    local x = toNumber(a.x, 0.0)
    local y = toNumber(a.y, 0.0)
    local z = toNumber(a.z, 0.0)
    return x * x + y * y + z * z
end

local function vnormalize(a)
    if type(a) == "table" and a.normalize then
        return a:normalize()
    end
    local l2 = vlen2(a)
    if l2 < 1e-12 then
        return vec3(1.0, 0.0, 0.0)
    end
    local inv = 1.0 / math.sqrt(l2)
    return vec3(toNumber(a.x, 0.0) * inv, toNumber(a.y, 0.0) * inv, toNumber(a.z, 0.0) * inv)
end

local function normalize2(x, y)
    local l = math.sqrt(x * x + y * y)
    if l < 1e-9 then
        return 1.0, 0.0
    end
    return x / l, y / l
end

local function getAxesWorld()
    local q = Phys.quaternionToWorld()


    local frontLocal = Block.frontLocal()
    local leftLocal = Block.leftLocal()
    local upLocal = Block.upLocal()

    if type(q) == "table" and q.transform
        and type(frontLocal) == "table"
        and type(leftLocal) == "table"
        and type(upLocal) == "table"
    then
        local f = q:transform(frontLocal)
        local l = q:transform(leftLocal)
        local u = q:transform(upLocal)
        if type(f) == "table" and type(l) == "table" and type(u) == "table" then
            return f, l, u
        end
    end

    local f = Block and Block.front and Block.front() or nil
    local l = Block and Block.left and Block.left() or nil
    local u = Block and Block.up and Block.up() or nil
    return f, l, u
end

local function getPitchDeg(frontWorld)
    if type(frontWorld) ~= "table" then
        return 0.0
    end
    local fy = clamp(toNumber(frontWorld.y, 0.0), -1.0, 1.0)
    return math.asin(fy) * RAD2DEG
end

local function computeHorizonBasis(frontWorld, leftWorld, upWorld, pitchDeg, pxPerDeg, cx, cy)
    if type(frontWorld) ~= "table" or type(leftWorld) ~= "table" or type(upWorld) ~= "table" then
        return 1.0, 0.0, cx, cy + pitchDeg * pxPerDeg
    end

    local worldUp = vec3(0.0, 1.0, 0.0)
    local rightWorld = vneg(leftWorld)
    local downWorld = vneg(upWorld)

    -- Direction of intersection: screen plane normal x world-up normal
    local hDir = vcross(frontWorld, worldUp)
    if vlen2(hDir) < 1e-10 then
        hDir = rightWorld
    end
    hDir = vnormalize(hDir)

    -- Project horizon direction to screen pixel coordinates
    -- x+: right, y+: down
    local tx = vdot(hDir, rightWorld)
    local ty = vdot(hDir, downWorld)
    tx, ty = normalize2(tx, ty)

    -- Screen-space normal of the horizon line
    local nx, ny = -ty, tx

    -- Keep +normal toward projected world-down (opposite of world-up),
    -- so positive pitch moves the horizon down on screen.
    local upProjX = vdot(worldUp, rightWorld)
    local upProjY = vdot(worldUp, downWorld)
    if nx * upProjX + ny * upProjY > 0 then
        nx, ny = -nx, -ny
    end

    local shift = pitchDeg * pxPerDeg
    local px = cx + nx * shift
    local py = cy + ny * shift
    return tx, ty, px, py
end

local function drawLineByX(x0, x1, px, py, tx, ty, thick, yMin, yMax)
    if math.abs(tx) < 1e-6 then
        local x = math.floor(px)
        local y0 = math.max(yMin, math.floor(py - math.abs(x1 - x0)))
        local y1 = math.min(yMax, math.floor(py + math.abs(x1 - x0)))
        for y = y0, y1 do
            render.drawRect(x, y, math.max(1, thick), 1)
        end
        return
    end

    local slope = ty / tx
    for x = x0, x1 do
        local y = py + (x - px) * slope
        if y >= yMin and y <= yMax then
            render.drawRect(x, math.floor(y), 1, math.max(1, thick))
        end
    end
end

local function drawPitchScale(cy, leftX, rightX, pitchDeg, pxPerDeg, h)
    for deg = -90, 90, 5 do
        local y = cy + (pitchDeg - deg) * pxPerDeg
        if y >= -8 and y <= h + 8 then
            local major = (deg % 10 == 0)
            local tickLen = major and 14 or 8
            local thick = major and 2 or 1
            local yi = math.floor(y)

            render.setColor(220, 240, 255, 200)
            render.drawRect(leftX, yi, tickLen, thick)
            render.drawRect(rightX - tickLen, yi, tickLen, thick)

            if major and deg ~= 0 then
                render.setColor(255, 255, 255, 235)
                render.drawText(tostring(deg), leftX - 22, yi - 4, 0.65)
                render.drawText(tostring(deg), rightX + 4, yi - 4, 0.65)
            end
        end
    end
end

function onClientTick()
    local w, h = getCanvasSize()
    local cx = math.floor(w * 0.5)
    local cy = math.floor(h * 0.5)

    local leftX = math.floor(w * 0.25)
    local rightX = math.floor(w * 0.75)
    local barTop = math.floor(h * 0.12)
    local barBottom = math.floor(h * 0.88)
    local barHeight = math.max(1, barBottom - barTop)

    local frontWorld, leftWorld, upWorld = getAxesWorld()
    local pitchDeg = getPitchDeg(frontWorld)
    local pxPerDeg = h / 60.0

    local tx, ty, horizonX, horizonY = computeHorizonBasis(
        frontWorld, leftWorld, upWorld, pitchDeg, pxPerDeg, cx, cy
    )

    render.clear()

    render.setColor(10, 18, 28, 255)
    render.setOpacity(0.55)
    render.drawRect(0, 0, w, h)
    render.setOpacity(1.0)

    render.setColor(110, 175, 255, 190)
    render.drawRect(leftX, barTop, 2, barHeight)
    render.drawRect(rightX, barTop, 2, barHeight)

    drawPitchScale(cy, leftX, rightX, pitchDeg, pxPerDeg, h)

    local x0 = leftX + 22
    local x1 = rightX - 22
    render.setColor(255, 235, 120, 230)
    drawLineByX(x0, x1, horizonX, horizonY, tx, ty, 2, barTop - 10, barBottom + 10)

    local yAtCenter
    if math.abs(tx) < 1e-6 then
        yAtCenter = horizonY
    else
        yAtCenter = horizonY + (cx - horizonX) * (ty / tx)
    end
    render.drawRect(cx - 6, math.floor(yAtCenter) - 2, 12, 6)

    render.pushLayer()
    render.setColor(255, 255, 255, 245)
    render.drawText("ATTITUDE", cx - 28, 8, 0.8)
    render.drawText(string.format("PITCH %+.1f", pitchDeg), cx - 34, h - 16, 0.75)

    render.submit()
end
