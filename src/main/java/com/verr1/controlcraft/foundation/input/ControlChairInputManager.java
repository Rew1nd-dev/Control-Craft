package com.verr1.controlcraft.foundation.input;

import com.verr1.controlcraft.foundation.data.WorldBlockPos;

import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public final class ControlChairInputManager {
    private static final ConcurrentMap<UUID, ControlChairSession> PLAYER_TO_SESSION = new ConcurrentHashMap<>();
    private static final ConcurrentMap<WorldBlockPos, UUID> CHAIR_TO_PLAYER = new ConcurrentHashMap<>();

    private ControlChairInputManager() {}

    public static ControlChairSession bind(UUID playerId, WorldBlockPos chairPos) {
        releasePlayer(playerId);

        UUID previousPlayer = CHAIR_TO_PLAYER.put(chairPos, playerId);
        if (previousPlayer != null && !previousPlayer.equals(playerId)) {
            PLAYER_TO_SESSION.remove(previousPlayer);
        }

        ControlChairSession session = new ControlChairSession(playerId, chairPos);
        PLAYER_TO_SESSION.put(playerId, session);
        return session;
    }

    public static ControlChairSession ensureBound(UUID playerId, WorldBlockPos chairPos) {
        ControlChairSession current = PLAYER_TO_SESSION.get(playerId);
        if (current != null && current.chairPos().equals(chairPos)) {
            CHAIR_TO_PLAYER.put(chairPos, playerId);
            return current;
        }
        return bind(playerId, chairPos);
    }

    public static boolean accept(UUID playerId, ControlChairInputDelta delta) {
        ControlChairSession session = PLAYER_TO_SESSION.get(playerId);
        if (session == null) {
            return false;
        }
        session.accept(delta);
        return true;
    }

    public static Optional<ControlChairSession> sessionOf(UUID playerId) {
        return Optional.ofNullable(PLAYER_TO_SESSION.get(playerId));
    }

    public static Optional<ControlChairSession> sessionAt(WorldBlockPos chairPos) {
        UUID playerId = CHAIR_TO_PLAYER.get(chairPos);
        if (playerId == null) {
            return Optional.empty();
        }
        return sessionOf(playerId);
    }

    public static void releasePlayer(UUID playerId) {
        ControlChairSession removed = PLAYER_TO_SESSION.remove(playerId);
        if (removed != null) {
            CHAIR_TO_PLAYER.remove(removed.chairPos(), playerId);
        }
    }

    public static void releaseChair(WorldBlockPos chairPos) {
        UUID playerId = CHAIR_TO_PLAYER.remove(chairPos);
        if (playerId != null) {
            PLAYER_TO_SESSION.remove(playerId);
        }
    }

    public static void clear() {
        PLAYER_TO_SESSION.clear();
        CHAIR_TO_PLAYER.clear();
    }
}
