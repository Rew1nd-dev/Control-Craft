-- NetworkLib full test script
--
-- Setup:
-- 1) Load this script into a computer with a screen.
-- 2) Make sure there is a Control Chair plant named "tweak" in the same constraint cluster.
-- 3) Sit in that chair and press button b8 to trigger one full test round.
--
-- What it covers:
-- - server -> client state sync
-- - server -> client event sync
-- - client authority request / canSend / release
-- - client -> server state send / sendMany / clear
-- - client -> server event emit
-- - compatibility aliases: Network.set / clear / peek / retrieve / send / isDirty

local CHAIR_NAME = "tweak"
local TRIGGER_PORT = "b8"

local CLIENT_TICK = 0
local SERVER_LAST_B8 = 0
local SERVER_RUN_ID = 0
local SERVER_TRIGGERS = 0

local CLIENT_INIT = false
local CLIENT_ACTIVE_RUN = 0
local CLIENT_REQUEST_RUN = 0
local CLIENT_SENT_RUN = 0
local CLIENT_RELEASE_RUN = 0
local CLIENT_LAST_REQUEST_TICK = -1000

local LOG_LINES = {}
local LOG_LIMIT = 10

local SERVER_STATE = {
    phase = "idle",
    triggerTick = 0,
    clientScalar = nil,
    clientLegacy = nil,
    clientManyA = nil,
    clientManyB = nil,
    clientClearSeen = false,
    clientClearRev = 0,
    clientEventSeq = 0,
    clientEventPeekSeq = 0,
    clientEventPayload = nil,
    clientSpareBefore = 0,
    clientSpareAfter = 0,
    clientSparePeekSeq = 0,
    serverEmitInfo = nil,
    serverSpareInfo = nil,
}

local CLIENT_VIEW = {
    runId = 0,
    serverScalar = nil,
    serverLegacy = nil,
    serverTouchValue = nil,
    serverTouchRev = 0,
    serverClearSeen = false,
    serverClearRev = 0,
    serverMany = nil,
    serverSummary = nil,
    serverSummaryRev = 0,
    serverEventPeekSeq = 0,
    serverEventPollSeq = 0,
    serverEventPayload = nil,
    serverEventBacklog = 0,
    serverSpareBefore = 0,
    serverSpareAfter = 0,
    serverSparePeekSeq = 0,
    authority = nil,
    requestOk = nil,
    requestInfo = nil,
    releaseOk = nil,
    releaseInfo = nil,
    sendOk = nil,
    sendInfo = nil,
    sendManyOk = nil,
    sendManyInfo = nil,
    clearOk = nil,
    clearInfo = nil,
    eventOk = nil,
    eventInfo = nil,
    spareEventOk = nil,
    spareEventInfo = nil,
    legacySendOk = nil,
    legacySendInfo = nil,
}

local function n(v, fallback)
    if type(v) == "number" then
        return v
    end
    return fallback
end

local function b(v)
    return not not v
end

local function compact(value, maxLen)
    local ok, text = pcall(function()
        return TextUtil.serialize(value, {
            compact = true,
            sortKeys = true,
            maxDepth = 5
        })
    end)

    local out
    if ok then
        out = text
    else
        out = tostring(value)
    end

    maxLen = maxLen or 84
    if #out > maxLen then
        return string.sub(out, 1, maxLen - 3) .. "..."
    end
    return out
end

local function pushLog(text)
    table.insert(LOG_LINES, 1, text)
    while #LOG_LINES > LOG_LIMIT do
        table.remove(LOG_LINES)
    end
end

local function resetServerRound(runId, tickNow)
    SERVER_STATE = {
        phase = "triggered",
        triggerTick = tickNow,
        clientScalar = nil,
        clientLegacy = nil,
        clientManyA = nil,
        clientManyB = nil,
        clientClearSeen = false,
        clientClearRev = 0,
        clientEventSeq = 0,
        clientEventPeekSeq = 0,
        clientEventPayload = nil,
        clientSpareBefore = 0,
        clientSpareAfter = 0,
        clientSparePeekSeq = 0,
        serverEmitInfo = nil,
        serverSpareInfo = nil,
    }

    Network.state.set("test_run_id", runId)
    Network.state.set("server_scalar", "srv-" .. tostring(runId))
    Network.set("server_legacy", "legacy-srv-" .. tostring(runId))

    Network.state.setMany({
        server_many_a = runId,
        server_many_b = {
            side = "server",
            run = runId,
            ok = true
        },
        client_clear_me = "seed-" .. tostring(runId),
    })

    Network.state.set("server_touch_probe", "touch-" .. tostring(runId))
    Network.touch("server_touch_probe")

    Network.state.set("server_clear_probe", "clear-" .. tostring(runId))
    Network.clear("server_clear_probe")

    local emitOk, emitInfo = Network.event.emit("server_ping", {
        side = "server",
        run = runId,
        triggerTick = tickNow,
        chair = CHAIR_NAME,
        port = TRIGGER_PORT
    })

    local spareOk, spareInfo = Network.event.emit("server_spare", {
        side = "server",
        run = runId,
        note = "client_should_clear_this_queue"
    })

    SERVER_STATE.serverEmitInfo = {
        ok = emitOk,
        info = emitInfo
    }
    SERVER_STATE.serverSpareInfo = {
        ok = spareOk,
        info = spareInfo
    }
end

local function updateServerSummary()
    Network.state.set("server_summary", {
        run = SERVER_RUN_ID,
        triggers = SERVER_TRIGGERS,
        phase = SERVER_STATE.phase,
        chair = CHAIR_NAME,
        port = TRIGGER_PORT,
        triggerTick = SERVER_STATE.triggerTick,
        authority = Network.authority.get(),
        clientScalar = SERVER_STATE.clientScalar,
        clientLegacy = SERVER_STATE.clientLegacy,
        clientManyA = SERVER_STATE.clientManyA,
        clientManyB = SERVER_STATE.clientManyB,
        clientClearSeen = SERVER_STATE.clientClearSeen,
        clientClearRev = SERVER_STATE.clientClearRev,
        clientEventSeq = SERVER_STATE.clientEventSeq,
        clientEventPeekSeq = SERVER_STATE.clientEventPeekSeq,
        clientEventPayload = SERVER_STATE.clientEventPayload,
        clientSpareBefore = SERVER_STATE.clientSpareBefore,
        clientSpareAfter = SERVER_STATE.clientSpareAfter,
        clientSparePeekSeq = SERVER_STATE.clientSparePeekSeq,
        serverEmitInfo = SERVER_STATE.serverEmitInfo,
        serverSpareInfo = SERVER_STATE.serverSpareInfo,
    })
end

local function processServerInbound()
    if Network.isDirty("client_legacy") then
        SERVER_STATE.clientLegacy = Network.retrieve("client_legacy")
        SERVER_STATE.phase = "client_legacy_received"
    end

    if Network.state.changed("client_scalar") then
        SERVER_STATE.clientScalar = Network.state.consume("client_scalar")
        SERVER_STATE.phase = "client_scalar_received"
    end

    if Network.state.changed("client_many_a") then
        SERVER_STATE.clientManyA = Network.state.consume("client_many_a")
        SERVER_STATE.phase = "client_many_received"
    end

    if Network.state.changed("client_many_b") then
        SERVER_STATE.clientManyB = Network.state.consume("client_many_b")
        SERVER_STATE.phase = "client_many_received"
    end

    if Network.state.changed("client_clear_me") then
        local clearedValue = Network.state.consume("client_clear_me")
        SERVER_STATE.clientClearSeen = (clearedValue == nil)
        SERVER_STATE.clientClearRev = Network.state.revision("client_clear_me")
        SERVER_STATE.phase = "client_clear_received"
    end

    local pingBacklog = Network.event.backlog("client_ping")
    if pingBacklog > 0 then
        local peekEvent = Network.event.peek("client_ping")
        SERVER_STATE.clientEventPeekSeq = peekEvent and peekEvent.seq or 0
        while Network.event.backlog("client_ping") > 0 do
            local event = Network.event.poll("client_ping")
            if event then
                SERVER_STATE.clientEventSeq = event.seq or 0
                SERVER_STATE.clientEventPayload = event.payload
                SERVER_STATE.phase = "client_event_received"
            end
        end
    end

    local spareBacklog = Network.event.backlog("client_spare")
    if spareBacklog > 0 then
        local peekSpare = Network.event.peek("client_spare")
        SERVER_STATE.clientSpareBefore = spareBacklog
        SERVER_STATE.clientSparePeekSeq = peekSpare and peekSpare.seq or 0
        Network.event.clear("client_spare")
        SERVER_STATE.clientSpareAfter = Network.event.backlog("client_spare")
        SERVER_STATE.phase = "client_spare_cleared"
    end
end

function onServerTick()
    local tickNow = World.gameClock()
    local b8 = Bus.retrieve(CHAIR_NAME, TRIGGER_PORT)
    local pressed = b8 > 0.5 and 1 or 0

    if pressed == 1 and SERVER_LAST_B8 == 0 then
        SERVER_RUN_ID = SERVER_RUN_ID + 1
        SERVER_TRIGGERS = SERVER_TRIGGERS + 1
        resetServerRound(SERVER_RUN_ID, tickNow)
    end

    SERVER_LAST_B8 = pressed

    processServerInbound()
    updateServerSummary()
end

local function initClientOnce()
    if CLIENT_INIT then
        return
    end

    render.setResolution(384, 216)
    render.setSurfaceSize(2.0, 1.15)
    render.setOffset(0.0, 1.05, -0.5)
    CLIENT_INIT = true
end

local function beginClientRound(runId)
    CLIENT_ACTIVE_RUN = runId
    CLIENT_REQUEST_RUN = 0
    CLIENT_SENT_RUN = 0
    CLIENT_RELEASE_RUN = 0

    CLIENT_VIEW.requestOk = nil
    CLIENT_VIEW.requestInfo = nil
    CLIENT_VIEW.releaseOk = nil
    CLIENT_VIEW.releaseInfo = nil
    CLIENT_VIEW.sendOk = nil
    CLIENT_VIEW.sendInfo = nil
    CLIENT_VIEW.sendManyOk = nil
    CLIENT_VIEW.sendManyInfo = nil
    CLIENT_VIEW.clearOk = nil
    CLIENT_VIEW.clearInfo = nil
    CLIENT_VIEW.eventOk = nil
    CLIENT_VIEW.eventInfo = nil
    CLIENT_VIEW.spareEventOk = nil
    CLIENT_VIEW.spareEventInfo = nil
    CLIENT_VIEW.legacySendOk = nil
    CLIENT_VIEW.legacySendInfo = nil
    CLIENT_VIEW.serverEventPeekSeq = 0
    CLIENT_VIEW.serverEventPollSeq = 0
    CLIENT_VIEW.serverEventPayload = nil
    CLIENT_VIEW.serverEventBacklog = 0
    CLIENT_VIEW.serverSpareBefore = 0
    CLIENT_VIEW.serverSpareAfter = 0
    CLIENT_VIEW.serverSparePeekSeq = 0

    pushLog("triggered run " .. tostring(runId))
end

local function maybeRequestAuthority(runId)
    if runId <= 0 or CLIENT_REQUEST_RUN == runId then
        return
    end

    local ok, info = Network.authority.request()
    CLIENT_REQUEST_RUN = runId
    CLIENT_LAST_REQUEST_TICK = CLIENT_TICK
    CLIENT_VIEW.requestOk = ok
    CLIENT_VIEW.requestInfo = info
    pushLog("authority.request => " .. compact(info, 96))
end

local function maybeRetryAuthority(runId)
    if runId <= 0 or CLIENT_SENT_RUN == runId then
        return
    end

    if CLIENT_TICK - CLIENT_LAST_REQUEST_TICK < 12 then
        return
    end

    local auth = Network.authority.get()
    if auth and auth.state == "cooldown" then
        return
    end

    local ok, info = Network.authority.request()
    CLIENT_LAST_REQUEST_TICK = CLIENT_TICK
    CLIENT_VIEW.requestOk = ok
    CLIENT_VIEW.requestInfo = info
    pushLog("authority.retry => " .. compact(info, 96))
end

local function maybeSendClientPayload(runId)
    if runId <= 0 or CLIENT_SENT_RUN == runId then
        return
    end

    if not Network.authority.canSend() then
        return
    end

    local okLegacy, infoLegacy = Network.send("client_legacy", "legacy-cli-" .. tostring(runId))
    local okScalar, infoScalar = Network.state.send("client_scalar", "cli-" .. tostring(runId))
    local okMany, infoMany = Network.state.sendMany({
        client_many_a = runId * 100,
        client_many_b = {
            side = "client",
            run = runId,
            tick = CLIENT_TICK
        }
    })
    local okClear, infoClear = Network.state.clear("client_clear_me")
    local okEvent, infoEvent = Network.event.emit("client_ping", {
        side = "client",
        run = runId,
        tick = CLIENT_TICK,
        yaw = Player.yaw(0.0),
        pitch = Player.pitch(0.0)
    })
    local okSpare, infoSpare = Network.event.emit("client_spare", {
        side = "client",
        run = runId,
        note = "server_should_clear_this_queue"
    })

    CLIENT_SENT_RUN = runId
    CLIENT_VIEW.legacySendOk = okLegacy
    CLIENT_VIEW.legacySendInfo = infoLegacy
    CLIENT_VIEW.sendOk = okScalar
    CLIENT_VIEW.sendInfo = infoScalar
    CLIENT_VIEW.sendManyOk = okMany
    CLIENT_VIEW.sendManyInfo = infoMany
    CLIENT_VIEW.clearOk = okClear
    CLIENT_VIEW.clearInfo = infoClear
    CLIENT_VIEW.eventOk = okEvent
    CLIENT_VIEW.eventInfo = infoEvent
    CLIENT_VIEW.spareEventOk = okSpare
    CLIENT_VIEW.spareEventInfo = infoSpare

    pushLog("client uplink sent for run " .. tostring(runId))
end

local function maybeReleaseAuthority(runId)
    if runId <= 0 or CLIENT_SENT_RUN ~= runId or CLIENT_RELEASE_RUN == runId then
        return
    end

    local ok, info = Network.authority.release()
    CLIENT_RELEASE_RUN = runId
    CLIENT_VIEW.releaseOk = ok
    CLIENT_VIEW.releaseInfo = info
    pushLog("authority.release => " .. compact(info, 96))
end

local function observeServerState()
    local runChanged = Network.state.changed("test_run_id")
    if runChanged then
        local runId = Network.state.consume("test_run_id") or 0
        CLIENT_VIEW.runId = runId
        beginClientRound(runId)
    else
        CLIENT_VIEW.runId = Network.state.get("test_run_id") or CLIENT_VIEW.runId or 0
    end

    if Network.isDirty("server_legacy") then
        CLIENT_VIEW.serverLegacy = Network.retrieve("server_legacy")
        pushLog("server legacy => " .. tostring(CLIENT_VIEW.serverLegacy))
    else
        CLIENT_VIEW.serverLegacy = Network.peek("server_legacy") or CLIENT_VIEW.serverLegacy
    end

    if Network.state.changed("server_scalar") then
        CLIENT_VIEW.serverScalar = Network.state.consume("server_scalar")
        pushLog("server scalar => " .. tostring(CLIENT_VIEW.serverScalar))
    end

    if Network.state.changed("server_touch_probe") then
        CLIENT_VIEW.serverTouchValue = Network.state.get("server_touch_probe")
        CLIENT_VIEW.serverTouchRev = Network.state.revision("server_touch_probe")
        pushLog("touch probe rev => " .. tostring(CLIENT_VIEW.serverTouchRev))
    else
        CLIENT_VIEW.serverTouchRev = Network.state.revision("server_touch_probe")
        CLIENT_VIEW.serverTouchValue = Network.state.get("server_touch_probe")
    end

    if Network.state.changed("server_clear_probe") then
        local clearedValue = Network.state.consume("server_clear_probe")
        CLIENT_VIEW.serverClearSeen = (clearedValue == nil)
        CLIENT_VIEW.serverClearRev = Network.state.revision("server_clear_probe")
        pushLog("server clear probe changed")
    else
        CLIENT_VIEW.serverClearRev = Network.state.revision("server_clear_probe")
    end

    CLIENT_VIEW.serverMany = Network.state.getMany({
        "server_many_a",
        "server_many_b"
    })

    if Network.state.changed("server_summary") then
        CLIENT_VIEW.serverSummary = Network.state.get("server_summary")
        CLIENT_VIEW.serverSummaryRev = Network.state.revision("server_summary")
    else
        CLIENT_VIEW.serverSummary = Network.state.get("server_summary")
        CLIENT_VIEW.serverSummaryRev = Network.state.revision("server_summary")
    end
end

local function observeServerEvents()
    CLIENT_VIEW.serverEventBacklog = Network.event.backlog("server_ping")
    if CLIENT_VIEW.serverEventBacklog > 0 then
        local peekEvent = Network.event.peek("server_ping")
        CLIENT_VIEW.serverEventPeekSeq = peekEvent and peekEvent.seq or 0
        local polled = Network.event.poll("server_ping")
        if polled then
            CLIENT_VIEW.serverEventPollSeq = polled.seq or 0
            CLIENT_VIEW.serverEventPayload = polled.payload
            pushLog("server event seq => " .. tostring(CLIENT_VIEW.serverEventPollSeq))
        end
    end

    local spareBacklog = Network.event.backlog("server_spare")
    if spareBacklog > 0 then
        local peekSpare = Network.event.peek("server_spare")
        CLIENT_VIEW.serverSpareBefore = spareBacklog
        CLIENT_VIEW.serverSparePeekSeq = peekSpare and peekSpare.seq or 0
        Network.event.clear("server_spare")
        CLIENT_VIEW.serverSpareAfter = Network.event.backlog("server_spare")
        pushLog("server spare queue cleared")
    end
end

local function drawLine(text, x, y, scale)
    render.drawText(text, x, y, scale or 0.58)
end

local function drawScreen()
    local w = (render.getWidth and render.getWidth()) or render.width or 384
    local h = (render.getHeight and render.getHeight()) or render.height or 216

    render.clear()

    render.setColor(10, 14, 20, 255)
    render.setOpacity(0.92)
    render.drawRect(0, 0, w, h)
    render.setOpacity(1.0)

    render.setColor(70, 130, 220, 255)
    render.drawRect(8, 8, w - 16, h - 16)
    render.setColor(18, 24, 38, 255)
    render.drawRect(10, 10, w - 20, h - 20)

    render.setColor(230, 235, 245, 255)
    drawLine("NetworkLib Full Test", 18, 16, 0.78)
    drawLine("Trigger: Bus.retrieve(\"" .. CHAIR_NAME .. "\", \"" .. TRIGGER_PORT .. "\") rising edge", 18, 30, 0.54)

    local y = 46
    local step = 11

    drawLine("run=" .. tostring(CLIENT_VIEW.runId) .. "  clientTick=" .. tostring(CLIENT_TICK), 18, y); y = y + step

    local auth = Network.authority.get()
    CLIENT_VIEW.authority = auth
    drawLine("authority.canSend=" .. tostring(Network.authority.canSend()) .. "  auth=" .. compact(auth, 92), 18, y); y = y + step
    drawLine("request=" .. tostring(CLIENT_VIEW.requestOk) .. " " .. compact(CLIENT_VIEW.requestInfo, 92), 18, y); y = y + step
    drawLine("release=" .. tostring(CLIENT_VIEW.releaseOk) .. " " .. compact(CLIENT_VIEW.releaseInfo, 92), 18, y); y = y + step

    drawLine("server scalar=" .. tostring(CLIENT_VIEW.serverScalar), 18, y); y = y + step
    drawLine("server legacy=" .. tostring(CLIENT_VIEW.serverLegacy), 18, y); y = y + step
    drawLine("server many=" .. compact(CLIENT_VIEW.serverMany, 92), 18, y); y = y + step
    drawLine("touch rev=" .. tostring(CLIENT_VIEW.serverTouchRev) .. " value=" .. tostring(CLIENT_VIEW.serverTouchValue), 18, y); y = y + step
    drawLine("clear probe seen=" .. tostring(CLIENT_VIEW.serverClearSeen) .. " rev=" .. tostring(CLIENT_VIEW.serverClearRev), 18, y); y = y + step

    drawLine("event ping backlog=" .. tostring(CLIENT_VIEW.serverEventBacklog) ..
        " peek=" .. tostring(CLIENT_VIEW.serverEventPeekSeq) ..
        " poll=" .. tostring(CLIENT_VIEW.serverEventPollSeq), 18, y); y = y + step
    drawLine("event payload=" .. compact(CLIENT_VIEW.serverEventPayload, 92), 18, y); y = y + step
    drawLine("event clear before=" .. tostring(CLIENT_VIEW.serverSpareBefore) ..
        " after=" .. tostring(CLIENT_VIEW.serverSpareAfter) ..
        " peek=" .. tostring(CLIENT_VIEW.serverSparePeekSeq), 18, y); y = y + step

    drawLine("send=" .. tostring(CLIENT_VIEW.sendOk) .. " " .. compact(CLIENT_VIEW.sendInfo, 92), 18, y); y = y + step
    drawLine("sendMany=" .. tostring(CLIENT_VIEW.sendManyOk) .. " " .. compact(CLIENT_VIEW.sendManyInfo, 92), 18, y); y = y + step
    drawLine("clear=" .. tostring(CLIENT_VIEW.clearOk) .. " " .. compact(CLIENT_VIEW.clearInfo, 92), 18, y); y = y + step
    drawLine("event emit=" .. tostring(CLIENT_VIEW.eventOk) .. " " .. compact(CLIENT_VIEW.eventInfo, 92), 18, y); y = y + step
    drawLine("legacy send=" .. tostring(CLIENT_VIEW.legacySendOk) .. " " .. compact(CLIENT_VIEW.legacySendInfo, 92), 18, y); y = y + step

    drawLine("server summary rev=" .. tostring(CLIENT_VIEW.serverSummaryRev) .. " " .. compact(CLIENT_VIEW.serverSummary, 92), 18, y); y = y + step

    render.setColor(160, 210, 255, 255)
    drawLine("logs:", 18, y, 0.6)
    y = y + step

    render.setColor(220, 230, 240, 255)
    for i = 1, #LOG_LINES do
        drawLine(LOG_LINES[i], 24, y, 0.54)
        y = y + step
        if y > h - 20 then
            break
        end
    end

    render.submit()
end

function onClientTick()
    initClientOnce()
    CLIENT_TICK = CLIENT_TICK + 1

    observeServerState()
    observeServerEvents()

    local runId = CLIENT_VIEW.runId or 0
    maybeRequestAuthority(runId)
    maybeRetryAuthority(runId)
    maybeSendClientPayload(runId)
    maybeReleaseAuthority(runId)

    drawScreen()
end
