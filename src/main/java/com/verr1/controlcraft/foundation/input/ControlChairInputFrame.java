package com.verr1.controlcraft.foundation.input;

public record ControlChairInputFrame(
        long sequence,
        double[] axes,
        long heldButtonsMask,
        long pressedButtonsMask,
        long releasedButtonsMask,
        long capturedAtNanos
) {
    private static final double[] EMPTY_AXES = new double[ControlChairAxis.VALUES.length];
    private static final ControlChairInputFrame EMPTY =
            new ControlChairInputFrame(0L, EMPTY_AXES, 0L, 0L, 0L, 0L);

    public ControlChairInputFrame {
        axes = axes.clone();
    }

    public static ControlChairInputFrame empty() {
        return EMPTY;
    }

    public double axis(ControlChairAxis axis) {
        return axes[axis.ordinal()];
    }

    public boolean held(ControlChairButton button) {
        return (heldButtonsMask & button.mask()) != 0L;
    }

    public boolean pressed(ControlChairButton button) {
        return (pressedButtonsMask & button.mask()) != 0L;
    }

    public boolean released(ControlChairButton button) {
        return (releasedButtonsMask & button.mask()) != 0L;
    }

    public double[] axesCopy() {
        return axes.clone();
    }
}
