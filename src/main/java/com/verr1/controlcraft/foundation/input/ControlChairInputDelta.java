package com.verr1.controlcraft.foundation.input;

import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;

public final class ControlChairInputDelta {
    private final long sequence;
    private final boolean fullSnapshot;
    private final boolean acceptingInput;
    private final boolean thirdPersonView;
    private final double viewYaw;
    private final double viewPitch;
    private final EnumMap<ControlChairAxis, Double> axisUpdates;
    private final long pressedButtonsMask;
    private final long releasedButtonsMask;
    private final Long heldButtonsMask;

    private ControlChairInputDelta(
            long sequence,
            boolean fullSnapshot,
            boolean acceptingInput,
            boolean thirdPersonView,
            double viewYaw,
            double viewPitch,
            EnumMap<ControlChairAxis, Double> axisUpdates,
            long pressedButtonsMask,
            long releasedButtonsMask,
            Long heldButtonsMask
    ) {
        this.sequence = sequence;
        this.fullSnapshot = fullSnapshot;
        this.acceptingInput = acceptingInput;
        this.thirdPersonView = thirdPersonView;
        this.viewYaw = viewYaw;
        this.viewPitch = viewPitch;
        this.axisUpdates = new EnumMap<>(axisUpdates);
        this.pressedButtonsMask = pressedButtonsMask;
        this.releasedButtonsMask = releasedButtonsMask;
        this.heldButtonsMask = heldButtonsMask;
    }

    public static Builder builder(long sequence) {
        return new Builder(sequence);
    }

    public long sequence() {
        return sequence;
    }

    public boolean isFullSnapshot() {
        return fullSnapshot;
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

    public Map<ControlChairAxis, Double> axisUpdates() {
        return Collections.unmodifiableMap(axisUpdates);
    }

    public long pressedButtonsMask() {
        return pressedButtonsMask;
    }

    public long releasedButtonsMask() {
        return releasedButtonsMask;
    }

    public Long heldButtonsMask() {
        return heldButtonsMask;
    }

    public static final class Builder {
        private final long sequence;
        private final EnumMap<ControlChairAxis, Double> axisUpdates = new EnumMap<>(ControlChairAxis.class);
        private boolean acceptingInput = true;
        private boolean thirdPersonView = false;
        private double viewYaw = 0.0;
        private double viewPitch = 0.0;
        private long pressedButtonsMask = 0L;
        private long releasedButtonsMask = 0L;
        private Long heldButtonsMask = null;
        private boolean fullSnapshot = false;

        private Builder(long sequence) {
            this.sequence = sequence;
        }

        public Builder axis(ControlChairAxis axis, double value) {
            axisUpdates.put(axis, value);
            return this;
        }

        public Builder acceptingInput(boolean acceptingInput) {
            this.acceptingInput = acceptingInput;
            return this;
        }

        public Builder thirdPersonView(boolean thirdPersonView) {
            this.thirdPersonView = thirdPersonView;
            return this;
        }

        public Builder viewAngles(double viewYaw, double viewPitch) {
            this.viewYaw = viewYaw;
            this.viewPitch = viewPitch;
            return this;
        }

        public Builder press(ControlChairButton button) {
            pressedButtonsMask |= button.mask();
            return this;
        }

        public Builder pressedButtonsMask(long pressedButtonsMask) {
            this.pressedButtonsMask |= pressedButtonsMask;
            return this;
        }

        public Builder release(ControlChairButton button) {
            releasedButtonsMask |= button.mask();
            return this;
        }

        public Builder releasedButtonsMask(long releasedButtonsMask) {
            this.releasedButtonsMask |= releasedButtonsMask;
            return this;
        }

        public Builder heldButtonsMask(long heldButtonsMask) {
            this.heldButtonsMask = heldButtonsMask;
            return this;
        }

        public Builder fullSnapshot() {
            this.fullSnapshot = true;
            return this;
        }

        public ControlChairInputDelta build() {
            return new ControlChairInputDelta(
                    sequence,
                    fullSnapshot,
                    acceptingInput,
                    thirdPersonView,
                    viewYaw,
                    viewPitch,
                    axisUpdates,
                    pressedButtonsMask,
                    releasedButtonsMask,
                    heldButtonsMask
            );
        }
    }
}
