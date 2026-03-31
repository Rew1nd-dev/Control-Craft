-- Tick clock test demo
-- Works for both:
-- 1) computer server scripts via onServerTick()
-- 2) luacuit scripts via loop()

local YELL_DISTANCE = 24
local ANNOUNCE_INTERVAL = 20

local last_announced_game_clock = -1

local function maybeAnnounceClocks()
    local game_clock = World.gameClock()
    local phys_clock = World.physClock()

    if game_clock <= 0 then
        return
    end

    if game_clock == last_announced_game_clock then
        return
    end

    if (game_clock % ANNOUNCE_INTERVAL) ~= 0 then
        return
    end

    last_announced_game_clock = game_clock
    World.yell(
        YELL_DISTANCE,
        string.format("gameClock=%d physClock=%d", game_clock, phys_clock)
    )
end

function onServerTick()
    maybeAnnounceClocks()
end

function loop()
    maybeAnnounceClocks()
end

function define()
end
