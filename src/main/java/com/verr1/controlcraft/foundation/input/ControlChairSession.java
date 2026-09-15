package com.verr1.controlcraft.foundation.input;

import com.verr1.controlcraft.foundation.data.WorldBlockPos;

import java.util.UUID;

public final class ControlChairSession {
    private final UUID playerId;
    private final WorldBlockPos chairPos;
    private final ControlChairInputTimeline timeline = new ControlChairInputTimeline();
    private volatile long lastInputAtNanos = System.nanoTime();
    private volatile boolean acceptingInput = true;
    private volatile boolean thirdPersonView = false;
    private volatile double viewYaw = 0.0;
    private volatile double viewPitch = 0.0;

    public ControlChairSession(UUID playerId, WorldBlockPos chairPos) {
        this.playerId = playerId;
        this.chairPos = chairPos;
    }

    public UUID playerId() {
        return playerId;
    }

    public WorldBlockPos chairPos() {
        return chairPos;
    }

    public long lastInputAtNanos() {
        return lastInputAtNanos;
    }

    public boolean acceptingInput() {
        return acceptingInput;
    }

    public boolean thirdPersonView() {
        return thirdPersonView;
    }

    public double viewYaw() {
        return viewYaw;
    }

    public double viewPitch() {
        return viewPitch;
    }

    public void accept(ControlChairInputDelta delta) {
        acceptingInput = delta.acceptingInput();
        thirdPersonView = delta.thirdPersonView();
        viewYaw = delta.viewYaw();
        viewPitch = delta.viewPitch();
        timeline.apply(delta);
        lastInputAtNanos = System.nanoTime();
    }

    public ControlChairInputFrame snapshot() {
        return timeline.snapshot();
    }

    public ControlChairInputFrame latestFrame() {
        return timeline.snapshot();
    }

    public ControlChairInputFrame pollFrame() {
        return timeline.snapshotAndClearTransient();
    }

    public void resetInput() {
        timeline.reset();
        acceptingInput = false;
        lastInputAtNanos = System.nanoTime();
    }
}
