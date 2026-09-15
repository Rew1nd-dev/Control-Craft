package com.verr1.controlcraft.foundation.input;

import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.input.ControlChairBindingsProfile.AnalogBinding;
import com.verr1.controlcraft.foundation.input.ControlChairBindingsProfile.AxisBindings;
import com.verr1.controlcraft.foundation.input.ControlChairBindingsProfile.DigitalBinding;
import com.verr1.controlcraft.foundation.input.ControlChairBindingsProfile.DirectionBinding;
import com.verr1.controlcraft.foundation.managers.ClientControlChairManager;
import com.verr1.controlcraft.foundation.network.packets.specific.ControlChairInputPacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import net.minecraft.util.Mth;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.Nullable;
import org.lwjgl.glfw.GLFW;

import java.util.Arrays;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.DoubleAdder;
import java.util.concurrent.locks.LockSupport;

@OnlyIn(Dist.CLIENT)
public final class ControlChairClientInput {
    private static final long SEND_INTERVAL_NANOS = 16_666_667L;
    private static final long HEARTBEAT_NANOS = 100_000_000L;
    private static final long FULL_SNAPSHOT_NANOS = 1_000_000_000L;
    private static final double AXIS_EPSILON = 1.0E-4;
    private static final double ANGLE_EPSILON = 1.0E-3;
    private static final double MAX_MOUSE_RATE = 2_000.0;

    private static final DoubleAdder MOUSE_X = new DoubleAdder();
    private static final DoubleAdder MOUSE_Y = new DoubleAdder();
    private static final Set<Integer> PRESSED_KEYS = ConcurrentHashMap.newKeySet();
    private static final Set<Integer> PRESSED_MOUSE_BUTTONS = ConcurrentHashMap.newKeySet();
    private static final AtomicBoolean STARTED = new AtomicBoolean(false);
    private static volatile boolean hasLockedMouseBaseline = false;
    private static volatile double lastLockedMouseX = 0.0;
    private static volatile double lastLockedMouseY = 0.0;

    private static volatile boolean running = false;
    private static @Nullable Thread senderThread = null;
    private static volatile RenderState renderState = RenderState.inactive();

    private ControlChairClientInput() {
    }

    public static void init() {
        if (!STARTED.compareAndSet(false, true)) {
            return;
        }

        running = true;
        Thread thread = new Thread(ControlChairClientInput::runSenderLoop, "ControlCraft-ControlChairClientInput");
        thread.setDaemon(true);
        thread.start();
        senderThread = thread;
    }

    public static void stop() {
        running = false;
        Thread thread = senderThread;
        if (thread != null) {
            thread.interrupt();
        }
        senderThread = null;
        renderState = RenderState.inactive();
        clearTransientClientState();
    }

    public static RenderState renderState() {
        return renderState;
    }

    public static boolean onCapturedKeyInput(int key, boolean pressed) {
        updateSet(PRESSED_KEYS, key, pressed);
        return true;
    }

    public static boolean onCapturedMouseButton(int button, boolean pressed) {
        updateSet(PRESSED_MOUSE_BUTTONS, button, pressed);
        return true;
    }

    public static void captureMouseTurn(double dx, double dy) {
        resetMouseTurnBaseline();
        MOUSE_X.add(dx);
        MOUSE_Y.add(dy);
    }

    public static void captureLockedMouseTurn(double absoluteX, double absoluteY) {
        if (!hasLockedMouseBaseline) {
            hasLockedMouseBaseline = true;
            lastLockedMouseX = absoluteX;
            lastLockedMouseY = absoluteY;
            return;
        }

        MOUSE_X.add(absoluteX - lastLockedMouseX);
        MOUSE_Y.add(absoluteY - lastLockedMouseY);
        lastLockedMouseX = absoluteX;
        lastLockedMouseY = absoluteY;
    }

    public static void clearCapturedInputState() {
        PRESSED_KEYS.clear();
        PRESSED_MOUSE_BUTTONS.clear();
        resetMouseTurnBaseline();
        drainMouse();
    }

    public static void resetMouseTurnBaseline() {
        hasLockedMouseBaseline = false;
        lastLockedMouseX = 0.0;
        lastLockedMouseY = 0.0;
    }

    private static void runSenderLoop() {
        long lastSampleAt = System.nanoTime();
        long lastSendAt = 0L;
        long lastFullSnapshotAt = 0L;
        long sequence = 0L;

        SentState lastSentState = SentState.empty();
        @Nullable WorldBlockPos lastChairPos = null;
        boolean wasActive = false;

        while (running) {
            long now = System.nanoTime();
            long sampleDuration = Math.max(1L, now - lastSampleAt);
            lastSampleAt = now;

            Sample sample = sample(sampleDuration);
            updateRenderState(sample);
            if (!sample.active()) {
                if (wasActive) {
                    if (hasConnection()) {
                        ControlChairInputDelta resetDelta = buildResetDelta(++sequence, lastSentState);
                        ControlCraftPackets.getChannel().sendToServer(new ControlChairInputPacket(resetDelta));
                    }
                    lastSendAt = now;
                    lastFullSnapshotAt = now;
                }

                drainMouse();
                clearCapturedInputState();
                lastSentState = SentState.empty();
                lastChairPos = null;
                wasActive = false;
                parkUntilNextSample(now);
                continue;
            }

            boolean chairChanged = lastChairPos == null || !lastChairPos.equals(sample.chairPos());
            boolean forceFull = !wasActive || chairChanged || now - lastFullSnapshotAt >= FULL_SNAPSHOT_NANOS;
            ControlChairInputDelta delta = buildDelta(++sequence, sample, lastSentState, forceFull);

            boolean send =
                    forceFull
                            || hasPayload(delta, lastSentState)
                            || now - lastSendAt >= HEARTBEAT_NANOS;

            if (send && hasConnection()) {
                ControlCraftPackets.getChannel().sendToServer(new ControlChairInputPacket(delta));
                lastSendAt = now;
                if (forceFull) {
                    lastFullSnapshotAt = now;
                }
                lastSentState = SentState.of(
                        sample.axes(),
                        sample.heldButtonsMask(),
                        sample.acceptingInput(),
                        sample.thirdPersonView(),
                        sample.viewYaw(),
                        sample.viewPitch()
                );
                lastChairPos = sample.chairPos();
                wasActive = true;
            }

            parkUntilNextSample(now);
        }
    }

    private static Sample sample(long sampleDurationNanos) {
        ClientControlChairManager.Snapshot snapshot = ClientControlChairManager.snapshot();
        double rawMouseX = MOUSE_X.sumThenReset();
        double rawMouseY = MOUSE_Y.sumThenReset();

        if (!snapshot.active() || snapshot.chairPos() == null) {
            return Sample.inactive();
        }

        boolean acceptingInput = snapshot.acceptingInput();
        double[] axes = new double[ControlChairAxis.VALUES.length];
        long heldButtonsMask = 0L;

        if (acceptingInput) {
            MouseRates mouseRates = snapshot.viewLockActive()
                    ? mouseRates(snapshot, sampleDurationNanos, rawMouseX, rawMouseY)
                    : MouseRates.zero();
            ControlChairGamepadManager.Snapshot gamepad = ControlChairGamepadManager.snapshot();
            ControlChairBindingsProfile profile = ControlChairBindingsStore.current();

            axes[ControlChairAxis.YAW.ordinal()] = logicalAxisValue(profile.axisBindings(ControlChairAxis.YAW), gamepad, mouseRates);
            axes[ControlChairAxis.PITCH.ordinal()] = logicalAxisValue(profile.axisBindings(ControlChairAxis.PITCH), gamepad, mouseRates);
            axes[ControlChairAxis.ROLL.ordinal()] = logicalAxisValue(profile.axisBindings(ControlChairAxis.ROLL), gamepad, mouseRates);
            axes[ControlChairAxis.THRUST.ordinal()] = logicalAxisValue(profile.axisBindings(ControlChairAxis.THRUST), gamepad, mouseRates);
            heldButtonsMask = computeHeldButtonsMask(profile, gamepad);
        }

        return new Sample(
                true,
                snapshot.chairPos(),
                acceptingInput,
                snapshot.viewYaw(),
                snapshot.viewPitch(),
                snapshot.thirdPersonView(),
                axes,
                heldButtonsMask
        );
    }

    private static long computeHeldButtonsMask(ControlChairBindingsProfile profile, ControlChairGamepadManager.Snapshot gamepad) {
        long mask = 0L;
        for (ControlChairButton button : ControlChairButton.VALUES) {
            var bindings = profile.buttonBindings(button);
            if (isDigitalBindingDown(bindings.primary(), gamepad) || isDigitalBindingDown(bindings.secondary(), gamepad)) {
                mask |= button.mask();
            }
        }
        return mask;
    }

    private static double logicalAxisValue(
            AxisBindings binding,
            ControlChairGamepadManager.Snapshot gamepad,
            MouseRates mouseRates
    ) {
        double positive = directionValue(binding.positive(), gamepad, mouseRates);
        double negative = directionValue(binding.negative(), gamepad, mouseRates);
        double sensitivity = binding.sensitivity();
        return Mth.clamp((positive - negative) * sensitivity, -sensitivity, sensitivity);
    }

    private static double directionValue(
            DirectionBinding binding,
            ControlChairGamepadManager.Snapshot gamepad,
            MouseRates mouseRates
    ) {
        double analog = analogValue(binding.analogBinding(), gamepad, mouseRates);
        double digital = isDigitalBindingDown(binding.buttonBinding(), gamepad) ? 1.0 : 0.0;
        return clampUnit(Math.max(analog, digital));
    }

    private static double analogValue(
            AnalogBinding binding,
            ControlChairGamepadManager.Snapshot gamepad,
            MouseRates mouseRates
    ) {
        if (!binding.isBound()) {
            return 0.0;
        }

        return switch (binding.type()) {
            case NONE -> 0.0;
            case GAMEPAD_AXIS_POSITIVE -> positiveGamepadAxisValue(gamepad, binding.code());
            case GAMEPAD_AXIS_NEGATIVE -> negativeGamepadAxisValue(gamepad, binding.code());
            case MOUSE_YAW_POSITIVE -> Math.max(mouseRates.yawRate(), 0.0);
            case MOUSE_YAW_NEGATIVE -> Math.max(-mouseRates.yawRate(), 0.0);
            case MOUSE_PITCH_POSITIVE -> Math.max(mouseRates.pitchRate(), 0.0);
            case MOUSE_PITCH_NEGATIVE -> Math.max(-mouseRates.pitchRate(), 0.0);
        };
    }

    private static double positiveGamepadAxisValue(ControlChairGamepadManager.Snapshot gamepad, int axis) {
        double value = normalizedGamepadAxisValue(gamepad, axis);
        if (isTriggerAxis(gamepad, axis)) {
            return value;
        }
        return Math.max(value, 0.0);
    }

    private static double negativeGamepadAxisValue(ControlChairGamepadManager.Snapshot gamepad, int axis) {
        if (isTriggerAxis(gamepad, axis)) {
            return 0.0;
        }
        return Math.max(-normalizedGamepadAxisValue(gamepad, axis), 0.0);
    }

    private static double normalizedGamepadAxisValue(ControlChairGamepadManager.Snapshot gamepad, int axis) {
        double raw = gamepad.axisValue(axis);
        if (isTriggerAxis(gamepad, axis)) {
            raw = (raw + 1.0) * 0.5;
            return Mth.clamp(raw, 0.0, 1.0);
        }
        return Mth.clamp(raw, -1.0, 1.0);
    }

    private static boolean isTriggerAxis(ControlChairGamepadManager.Snapshot gamepad, int axis) {
        return gamepad.mappedGamepad()
                && (axis == GLFW.GLFW_GAMEPAD_AXIS_LEFT_TRIGGER || axis == GLFW.GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER);
    }

    private static boolean isDigitalBindingDown(DigitalBinding binding, ControlChairGamepadManager.Snapshot gamepad) {
        if (!binding.isBound()) {
            return false;
        }

        return switch (binding.type()) {
            case NONE -> false;
            case KEYBOARD_KEY -> isKeyDown(binding.code());
            case MOUSE_BUTTON -> PRESSED_MOUSE_BUTTONS.contains(binding.code());
            case GAMEPAD_BUTTON -> gamepad.buttonDown(binding.code());
        };
    }

    private static boolean isKeyDown(int key) {
        return switch (key) {
            case GLFW.GLFW_KEY_LEFT_SHIFT, GLFW.GLFW_KEY_RIGHT_SHIFT ->
                    PRESSED_KEYS.contains(GLFW.GLFW_KEY_LEFT_SHIFT) || PRESSED_KEYS.contains(GLFW.GLFW_KEY_RIGHT_SHIFT);
            case GLFW.GLFW_KEY_LEFT_CONTROL, GLFW.GLFW_KEY_RIGHT_CONTROL ->
                    PRESSED_KEYS.contains(GLFW.GLFW_KEY_LEFT_CONTROL) || PRESSED_KEYS.contains(GLFW.GLFW_KEY_RIGHT_CONTROL);
            case GLFW.GLFW_KEY_LEFT_ALT, GLFW.GLFW_KEY_RIGHT_ALT ->
                    PRESSED_KEYS.contains(GLFW.GLFW_KEY_LEFT_ALT) || PRESSED_KEYS.contains(GLFW.GLFW_KEY_RIGHT_ALT);
            default -> PRESSED_KEYS.contains(key);
        };
    }

    private static MouseRates mouseRates(
            ClientControlChairManager.Snapshot snapshot,
            long sampleDurationNanos,
            double rawMouseX,
            double rawMouseY
    ) {
        double seconds = sampleDurationNanos / 1_000_000_000.0;
        if (seconds <= 0.0 || seconds > 0.25) {
            return MouseRates.zero();
        }

        double scale = snapshot.mouseSensitivityScale() / seconds;
        double yawRate = Mth.clamp(rawMouseX * scale / MAX_MOUSE_RATE, -1.0, 1.0);
        double pitchRate = Mth.clamp(
                rawMouseY * scale * (snapshot.invertY() ? -1.0 : 1.0) / MAX_MOUSE_RATE,
                -1.0,
                1.0
        );
        return new MouseRates(yawRate, pitchRate);
    }

    private static double clampUnit(double value) {
        return Mth.clamp(value, 0.0, 1.0);
    }

    private static void updateSet(Set<Integer> set, int code, boolean pressed) {
        if (pressed) {
            set.add(code);
        } else {
            set.remove(code);
        }
    }

    private static ControlChairInputDelta buildDelta(
            long sequence,
            Sample sample,
            SentState lastSentState,
            boolean forceFull
    ) {
        ControlChairInputDelta.Builder builder = ControlChairInputDelta.builder(sequence)
                .acceptingInput(sample.acceptingInput())
                .thirdPersonView(sample.thirdPersonView())
                .viewAngles(sample.viewYaw(), sample.viewPitch());
        if (forceFull) {
            builder.fullSnapshot().heldButtonsMask(sample.heldButtonsMask());
        }

        for (ControlChairAxis axis : ControlChairAxis.VALUES) {
            double next = sample.axes()[axis.ordinal()];
            double previous = lastSentState.axes()[axis.ordinal()];
            if (forceFull || Math.abs(next - previous) > AXIS_EPSILON) {
                builder.axis(axis, next);
            }
        }

        long pressedButtonsMask = sample.heldButtonsMask() & ~lastSentState.heldButtonsMask();
        long releasedButtonsMask = lastSentState.heldButtonsMask() & ~sample.heldButtonsMask();

        builder.pressedButtonsMask(pressedButtonsMask);
        builder.releasedButtonsMask(releasedButtonsMask);
        return builder.build();
    }

    private static ControlChairInputDelta buildResetDelta(long sequence, SentState lastSentState) {
        ControlChairInputDelta.Builder builder = ControlChairInputDelta.builder(sequence)
                .acceptingInput(false)
                .thirdPersonView(lastSentState.thirdPersonView())
                .viewAngles(lastSentState.viewYaw(), lastSentState.viewPitch())
                .fullSnapshot()
                .heldButtonsMask(0L)
                .releasedButtonsMask(lastSentState.heldButtonsMask());

        for (ControlChairAxis axis : ControlChairAxis.VALUES) {
            builder.axis(axis, 0.0);
        }

        return builder.build();
    }

    private static boolean hasPayload(ControlChairInputDelta delta, SentState lastSentState) {
        return !delta.axisUpdates().isEmpty()
                || delta.pressedButtonsMask() != 0L
                || delta.releasedButtonsMask() != 0L
                || delta.acceptingInput() != lastSentState.acceptingInput()
                || delta.thirdPersonView() != lastSentState.thirdPersonView()
                || Math.abs(delta.viewYaw() - lastSentState.viewYaw()) > ANGLE_EPSILON
                || Math.abs(delta.viewPitch() - lastSentState.viewPitch()) > ANGLE_EPSILON;
    }

    private static boolean hasConnection() {
        return ClientControlChairManager.snapshot().active() && ControlCraftPackets.getChannel() != null;
    }

    private static void parkUntilNextSample(long sampleStartNanos) {
        long remaining = SEND_INTERVAL_NANOS - (System.nanoTime() - sampleStartNanos);
        if (remaining > 0L) {
            LockSupport.parkNanos(remaining);
        }
    }

    private static void clearTransientClientState() {
        drainMouse();
        clearCapturedInputState();
    }

    private static void updateRenderState(Sample sample) {
        if (!sample.active() || sample.chairPos() == null) {
            renderState = RenderState.inactive();
            return;
        }

        renderState = new RenderState(
                true,
                sample.chairPos(),
                sample.axes()[ControlChairAxis.ROLL.ordinal()],
                sample.axes()[ControlChairAxis.PITCH.ordinal()]
        );
    }

    private static void drainMouse() {
        MOUSE_X.sumThenReset();
        MOUSE_Y.sumThenReset();
    }

    private record Sample(
            boolean active,
            @Nullable WorldBlockPos chairPos,
            boolean acceptingInput,
            float viewYaw,
            float viewPitch,
            boolean thirdPersonView,
            double[] axes,
            long heldButtonsMask
    ) {
        private static Sample inactive() {
            return new Sample(false, null, false, 0.0F, 0.0F, false, new double[ControlChairAxis.VALUES.length], 0L);
        }
    }

    private record SentState(
            double[] axes,
            long heldButtonsMask,
            boolean acceptingInput,
            boolean thirdPersonView,
            float viewYaw,
            float viewPitch
    ) {
        private static SentState empty() {
            return new SentState(new double[ControlChairAxis.VALUES.length], 0L, false, false, 0.0F, 0.0F);
        }

        private static SentState of(
                double[] axes,
                long heldButtonsMask,
                boolean acceptingInput,
                boolean thirdPersonView,
                float viewYaw,
                float viewPitch
        ) {
            return new SentState(
                    Arrays.copyOf(axes, axes.length),
                    heldButtonsMask,
                    acceptingInput,
                    thirdPersonView,
                    viewYaw,
                    viewPitch
            );
        }
    }

    private record MouseRates(double yawRate, double pitchRate) {
        private static MouseRates zero() {
            return new MouseRates(0.0, 0.0);
        }
    }

    public record RenderState(
            boolean active,
            @Nullable WorldBlockPos chairPos,
            double roll,
            double pitch
    ) {
        private static RenderState inactive() {
            return new RenderState(false, null, 0.0, 0.0);
        }
    }
}
