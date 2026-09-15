package com.verr1.controlcraft.content.links.computer;

import org.jetbrains.annotations.Nullable;

import java.util.UUID;

public record ComputerAuthoritySnapshot(
        ComputerAuthorityMode mode,
        @Nullable UUID ownerId,
        long leaseExpireTick,
        long token
) {
    public static ComputerAuthoritySnapshot none() {
        return new ComputerAuthoritySnapshot(ComputerAuthorityMode.NONE, null, 0L, 0L);
    }

    public boolean isActive() {
        return mode != ComputerAuthorityMode.NONE && ownerId != null && token != 0L;
    }

    public boolean isOwnedBy(@Nullable UUID playerId, long gameTick) {
        if (playerId == null || ownerId == null || !ownerId.equals(playerId) || token == 0L) {
            return false;
        }
        if (mode == ComputerAuthorityMode.FORCED) {
            return true;
        }
        return mode == ComputerAuthorityMode.LEASED && gameTick <= leaseExpireTick;
    }
}
