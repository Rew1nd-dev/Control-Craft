package com.verr1.controlcraft.foundation.input;

import org.jetbrains.annotations.Nullable;
import org.lwjgl.glfw.GLFW;
import org.lwjgl.glfw.GLFWGamepadState;

import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.util.Arrays;

public final class ControlChairGamepadManager {
    private static final GLFWGamepadState GLFW_STATE = GLFWGamepadState.create();
    private static volatile Snapshot snapshot = Snapshot.disconnected();

    private ControlChairGamepadManager() {
    }

    public static void poll() {
        for (int joystick = GLFW.GLFW_JOYSTICK_1; joystick <= GLFW.GLFW_JOYSTICK_LAST; joystick++) {
            if (!GLFW.glfwJoystickPresent(joystick)) {
                continue;
            }

            if (GLFW.glfwJoystickIsGamepad(joystick) && GLFW.glfwGetGamepadState(joystick, GLFW_STATE)) {
                boolean[] buttons = new boolean[GLFW.GLFW_GAMEPAD_BUTTON_LAST + 1];
                float[] axes = new float[GLFW.GLFW_GAMEPAD_AXIS_LAST + 1];

                for (int button = 0; button < buttons.length; button++) {
                    buttons[button] = GLFW_STATE.buttons(button) == GLFW.GLFW_PRESS;
                }
                for (int axis = 0; axis < axes.length; axis++) {
                    axes[axis] = GLFW_STATE.axes(axis);
                }

                snapshot = new Snapshot(
                        true,
                        true,
                        joystick,
                        GLFW.glfwGetGamepadName(joystick),
                        buttons,
                        axes
                );
                return;
            }

            ByteBuffer rawButtons = GLFW.glfwGetJoystickButtons(joystick);
            FloatBuffer rawAxes = GLFW.glfwGetJoystickAxes(joystick);
            if (rawButtons == null && rawAxes == null) {
                continue;
            }

            boolean[] buttons = new boolean[rawButtons == null ? 0 : rawButtons.remaining()];
            float[] axes = new float[rawAxes == null ? 0 : rawAxes.remaining()];

            for (int button = 0; button < buttons.length; button++) {
                buttons[button] = rawButtons.get(button) == GLFW.GLFW_PRESS;
            }
            for (int axis = 0; axis < axes.length; axis++) {
                axes[axis] = rawAxes.get(axis);
            }

            snapshot = new Snapshot(
                    true,
                    false,
                    joystick,
                    GLFW.glfwGetJoystickName(joystick),
                    buttons,
                    axes
            );
            return;
        }

        snapshot = Snapshot.disconnected();
    }

    public static Snapshot snapshot() {
        return snapshot;
    }

    public static String buttonLabel(int button) {
        return switch (button) {
            case GLFW.GLFW_GAMEPAD_BUTTON_A -> "Pad A";
            case GLFW.GLFW_GAMEPAD_BUTTON_B -> "Pad B";
            case GLFW.GLFW_GAMEPAD_BUTTON_X -> "Pad X";
            case GLFW.GLFW_GAMEPAD_BUTTON_Y -> "Pad Y";
            case GLFW.GLFW_GAMEPAD_BUTTON_LEFT_BUMPER -> "Pad LB";
            case GLFW.GLFW_GAMEPAD_BUTTON_RIGHT_BUMPER -> "Pad RB";
            case GLFW.GLFW_GAMEPAD_BUTTON_BACK -> "Pad Back";
            case GLFW.GLFW_GAMEPAD_BUTTON_START -> "Pad Start";
            case GLFW.GLFW_GAMEPAD_BUTTON_GUIDE -> "Pad Guide";
            case GLFW.GLFW_GAMEPAD_BUTTON_LEFT_THUMB -> "Pad LS";
            case GLFW.GLFW_GAMEPAD_BUTTON_RIGHT_THUMB -> "Pad RS";
            case GLFW.GLFW_GAMEPAD_BUTTON_DPAD_UP -> "Pad Up";
            case GLFW.GLFW_GAMEPAD_BUTTON_DPAD_RIGHT -> "Pad Right";
            case GLFW.GLFW_GAMEPAD_BUTTON_DPAD_DOWN -> "Pad Down";
            case GLFW.GLFW_GAMEPAD_BUTTON_DPAD_LEFT -> "Pad Left";
            default -> "Pad Button " + button;
        };
    }

    public static String axisLabel(int axis) {
        return switch (axis) {
            case GLFW.GLFW_GAMEPAD_AXIS_LEFT_X -> "Pad LX";
            case GLFW.GLFW_GAMEPAD_AXIS_LEFT_Y -> "Pad LY";
            case GLFW.GLFW_GAMEPAD_AXIS_RIGHT_X -> "Pad RX";
            case GLFW.GLFW_GAMEPAD_AXIS_RIGHT_Y -> "Pad RY";
            case GLFW.GLFW_GAMEPAD_AXIS_LEFT_TRIGGER -> "Pad LT";
            case GLFW.GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER -> "Pad RT";
            default -> "Pad Axis " + axis;
        };
    }

    public record Snapshot(
            boolean connected,
            boolean mappedGamepad,
            int joystickId,
            @Nullable String name,
            boolean[] buttons,
            float[] axes
    ) {
        public static Snapshot disconnected() {
            return new Snapshot(false, false, -1, null, new boolean[0], new float[0]);
        }

        public boolean buttonDown(int button) {
            return button >= 0 && button < buttons.length && buttons[button];
        }

        public float axisValue(int axis) {
            return axis >= 0 && axis < axes.length ? axes[axis] : 0.0F;
        }

        public Snapshot copy() {
            return new Snapshot(connected, mappedGamepad, joystickId, name, Arrays.copyOf(buttons, buttons.length), Arrays.copyOf(axes, axes.length));
        }
    }
}
