package com.verr1.controlcraft.utils;

import java.util.concurrent.atomic.AtomicLong;

public final class GlobalTickClock {

    private static final AtomicLong GAME_TICK = new AtomicLong(0);
    private static final AtomicLong PHYS_TICK = new AtomicLong(0);

    private GlobalTickClock() {
    }

    public static void reset() {
        GAME_TICK.set(0);
        PHYS_TICK.set(0);
    }

    public static long tickGame() {
        return GAME_TICK.incrementAndGet();
    }

    public static long tickPhys() {
        return PHYS_TICK.incrementAndGet();
    }

    public static long gameClock() {
        return GAME_TICK.get();
    }

    public static long physClock() {
        return PHYS_TICK.get();
    }
}
