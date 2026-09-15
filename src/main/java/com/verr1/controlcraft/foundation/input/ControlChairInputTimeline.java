package com.verr1.controlcraft.foundation.input;

import java.util.Arrays;

public final class ControlChairInputTimeline {
    private final Object lock = new Object();

    private long sequence = 0L;
    private final double[] axes = new double[ControlChairAxis.VALUES.length];
    private long heldButtonsMask = 0L;
    private long pressedButtonsMask = 0L;
    private long releasedButtonsMask = 0L;
    private volatile ControlChairInputFrame publishedFrame = ControlChairInputFrame.empty();

    public void apply(ControlChairInputDelta delta) {
        synchronized (lock) {
            if (delta.sequence() < sequence) {
                return;
            }

            sequence = Math.max(sequence, delta.sequence());

            for (var entry : delta.axisUpdates().entrySet()) {
                axes[entry.getKey().ordinal()] = entry.getValue();
            }

            if (delta.isFullSnapshot() && delta.heldButtonsMask() != null) {
                heldButtonsMask = delta.heldButtonsMask();
            }

            pressedButtonsMask |= delta.pressedButtonsMask();
            releasedButtonsMask |= delta.releasedButtonsMask();

            heldButtonsMask |= delta.pressedButtonsMask();
            heldButtonsMask &= ~delta.releasedButtonsMask();
            publishedFrame = captureFrame();
        }
    }

    public ControlChairInputFrame snapshot() {
        return publishedFrame;
    }

    public ControlChairInputFrame snapshotAndClearTransient() {
        synchronized (lock) {
            ControlChairInputFrame frame = captureFrame();
            pressedButtonsMask = 0L;
            releasedButtonsMask = 0L;
            publishedFrame = captureFrame();
            return frame;
        }
    }

    public void reset() {
        synchronized (lock) {
            sequence = 0L;
            Arrays.fill(axes, 0.0);
            heldButtonsMask = 0L;
            pressedButtonsMask = 0L;
            releasedButtonsMask = 0L;
            publishedFrame = ControlChairInputFrame.empty();
        }
    }

    private ControlChairInputFrame captureFrame() {
        return new ControlChairInputFrame(
                sequence,
                axes,
                heldButtonsMask,
                pressedButtonsMask,
                releasedButtonsMask,
                System.nanoTime()
        );
    }
}
