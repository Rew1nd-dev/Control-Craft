package com.verr1.controlcraft.foundation.input;

import com.google.gson.JsonObject;
import org.lwjgl.glfw.GLFW;

import java.util.EnumMap;

public final class ControlChairBindingsProfile {
    public static final double MIN_SENSITIVITY = 0.1;
    public static final double MAX_SENSITIVITY = 5.0;
    public static final double DEFAULT_SENSITIVITY = 1.0;

    private final EnumMap<ControlChairAxis, AxisBindings> axisBindings = new EnumMap<>(ControlChairAxis.class);
    private final EnumMap<ControlChairButton, ButtonBindings> buttonBindings = new EnumMap<>(ControlChairButton.class);

    public ControlChairBindingsProfile() {
        for (ControlChairAxis axis : ControlChairAxis.VALUES) {
            axisBindings.put(axis, AxisBindings.unbound());
        }
        for (ControlChairButton button : ControlChairButton.VALUES) {
            buttonBindings.put(button, ButtonBindings.unbound());
        }
    }

    public static ControlChairBindingsProfile defaults() {
        ControlChairBindingsProfile profile = new ControlChairBindingsProfile();

        profile.setDirectionAnalogBinding(AxisDirection.YAW_POS, AnalogBinding.mouseYawPositive());
        profile.setDirectionAnalogBinding(AxisDirection.YAW_NEG, AnalogBinding.mouseYawNegative());
        profile.setDirectionAnalogBinding(AxisDirection.PITCH_POS, AnalogBinding.mousePitchPositive());
        profile.setDirectionAnalogBinding(AxisDirection.PITCH_NEG, AnalogBinding.mousePitchNegative());
        profile.setDirectionButtonBinding(AxisDirection.ROLL_POS, DigitalBinding.key(GLFW.GLFW_KEY_E));
        profile.setDirectionButtonBinding(AxisDirection.ROLL_NEG, DigitalBinding.key(GLFW.GLFW_KEY_Q));
        profile.setDirectionButtonBinding(AxisDirection.THRUST_POS, DigitalBinding.key(GLFW.GLFW_KEY_W));
        profile.setDirectionButtonBinding(AxisDirection.THRUST_NEG, DigitalBinding.key(GLFW.GLFW_KEY_S));

        profile.setButtonBindings(ControlChairButton.B0, new ButtonBindings(
                DigitalBinding.mouse(GLFW.GLFW_MOUSE_BUTTON_LEFT),
                DigitalBinding.unbound()
        ));
        profile.setButtonBindings(ControlChairButton.B1, new ButtonBindings(
                DigitalBinding.mouse(GLFW.GLFW_MOUSE_BUTTON_RIGHT),
                DigitalBinding.unbound()
        ));
        profile.setButtonBindings(ControlChairButton.B2, new ButtonBindings(
                DigitalBinding.key(GLFW.GLFW_KEY_SPACE),
                DigitalBinding.unbound()
        ));
        profile.setButtonBindings(ControlChairButton.B3, new ButtonBindings(
                DigitalBinding.key(GLFW.GLFW_KEY_LEFT_SHIFT),
                DigitalBinding.key(GLFW.GLFW_KEY_RIGHT_SHIFT)
        ));
        profile.setButtonBindings(ControlChairButton.B4, new ButtonBindings(
                DigitalBinding.key(GLFW.GLFW_KEY_LEFT_CONTROL),
                DigitalBinding.key(GLFW.GLFW_KEY_RIGHT_CONTROL)
        ));
        profile.setButtonBindings(ControlChairButton.B5, new ButtonBindings(
                DigitalBinding.mouse(GLFW.GLFW_MOUSE_BUTTON_MIDDLE),
                DigitalBinding.unbound()
        ));
        profile.setButtonBindings(ControlChairButton.B6, new ButtonBindings(
                DigitalBinding.key(GLFW.GLFW_KEY_W),
                DigitalBinding.unbound()
        ));
        profile.setButtonBindings(ControlChairButton.B7, new ButtonBindings(
                DigitalBinding.key(GLFW.GLFW_KEY_S),
                DigitalBinding.unbound()
        ));
        profile.setButtonBindings(ControlChairButton.B8, new ButtonBindings(
                DigitalBinding.key(GLFW.GLFW_KEY_A),
                DigitalBinding.unbound()
        ));
        profile.setButtonBindings(ControlChairButton.B9, new ButtonBindings(
                DigitalBinding.key(GLFW.GLFW_KEY_D),
                DigitalBinding.unbound()
        ));
        profile.setButtonBindings(ControlChairButton.B10, new ButtonBindings(
                DigitalBinding.key(GLFW.GLFW_KEY_Q),
                DigitalBinding.unbound()
        ));
        profile.setButtonBindings(ControlChairButton.B11, new ButtonBindings(
                DigitalBinding.key(GLFW.GLFW_KEY_E),
                DigitalBinding.unbound()
        ));

        return profile;
    }

    public ControlChairBindingsProfile copy() {
        ControlChairBindingsProfile copy = new ControlChairBindingsProfile();
        for (ControlChairAxis axis : ControlChairAxis.VALUES) {
            copy.axisBindings.put(axis, axisBindings.get(axis).copy());
        }
        for (ControlChairButton button : ControlChairButton.VALUES) {
            copy.buttonBindings.put(button, buttonBindings.get(button).copy());
        }
        return copy;
    }

    public AxisBindings axisBindings(ControlChairAxis axis) {
        return axisBindings.get(axis);
    }

    public DirectionBinding directionBinding(AxisDirection direction) {
        AxisBindings axis = axisBindings(direction.axis());
        return direction.positive() ? axis.positive() : axis.negative();
    }

    public double axisSensitivity(ControlChairAxis axis) {
        return axisBindings(axis).sensitivity();
    }

    public ButtonBindings buttonBindings(ControlChairButton button) {
        return buttonBindings.get(button);
    }

    public void setDirectionAnalogBinding(AxisDirection direction, AnalogBinding binding) {
        AxisBindings current = axisBindings(direction.axis());
        DirectionBinding nextDirection = directionBinding(direction).withAnalogBinding(binding);
        axisBindings.put(direction.axis(), direction.positive()
                ? current.withPositive(nextDirection)
                : current.withNegative(nextDirection));
    }

    public void setDirectionButtonBinding(AxisDirection direction, DigitalBinding binding) {
        AxisBindings current = axisBindings(direction.axis());
        DirectionBinding nextDirection = directionBinding(direction).withButtonBinding(binding);
        axisBindings.put(direction.axis(), direction.positive()
                ? current.withPositive(nextDirection)
                : current.withNegative(nextDirection));
    }

    public void setAxisSensitivity(ControlChairAxis axis, double sensitivity) {
        axisBindings.put(axis, axisBindings(axis).withSensitivity(sensitivity));
    }

    public void setButtonPrimaryBinding(ControlChairButton button, DigitalBinding binding) {
        ButtonBindings current = buttonBindings(button);
        buttonBindings.put(button, new ButtonBindings(binding, current.secondary()));
    }

    public void setButtonSecondaryBinding(ControlChairButton button, DigitalBinding binding) {
        ButtonBindings current = buttonBindings(button);
        buttonBindings.put(button, new ButtonBindings(current.primary(), binding));
    }

    public void setButtonBindings(ControlChairButton button, ButtonBindings bindings) {
        buttonBindings.put(button, bindings);
    }

    public JsonObject toJson() {
        JsonObject root = new JsonObject();
        JsonObject axes = new JsonObject();
        JsonObject buttons = new JsonObject();

        for (ControlChairAxis axis : ControlChairAxis.VALUES) {
            axes.add(axis.serializedName(), axisBindings(axis).toJson());
        }
        for (ControlChairButton button : ControlChairButton.VALUES) {
            buttons.add(button.serializedName(), buttonBindings(button).toJson());
        }

        root.add("axes", axes);
        root.add("buttons", buttons);
        return root;
    }

    public static ControlChairBindingsProfile fromJson(JsonObject root) {
        ControlChairBindingsProfile profile = defaults();
        if (root == null) {
            return profile;
        }

        if (root.has("axes") && root.get("axes").isJsonObject()) {
            JsonObject axes = root.getAsJsonObject("axes");
            for (ControlChairAxis axis : ControlChairAxis.VALUES) {
                if (axes.has(axis.serializedName()) && axes.get(axis.serializedName()).isJsonObject()) {
                    profile.axisBindings.put(axis, AxisBindings.fromJson(axes.getAsJsonObject(axis.serializedName())));
                } else {
                    AxisBindings legacy = legacyAxisBindings(axes, axis);
                    if (legacy != null) {
                        profile.axisBindings.put(axis, legacy);
                    }
                }
            }
        }

        if (root.has("buttons") && root.get("buttons").isJsonObject()) {
            JsonObject buttons = root.getAsJsonObject("buttons");
            for (ControlChairButton button : ControlChairButton.VALUES) {
                if (buttons.has(button.serializedName()) && buttons.get(button.serializedName()).isJsonObject()) {
                    profile.buttonBindings.put(button, ButtonBindings.fromJson(buttons.getAsJsonObject(button.serializedName())));
                }
            }
        }

        return profile;
    }

    private static AxisBindings legacyAxisBindings(JsonObject legacyAxes, ControlChairAxis axis) {
        String positiveKey;
        String negativeKey;

        switch (axis) {
            case YAW -> {
                positiveKey = "yaw_pos";
                negativeKey = "yaw_neg";
            }
            case PITCH -> {
                positiveKey = "pitch_pos";
                negativeKey = "pitch_neg";
            }
            case ROLL -> {
                positiveKey = "roll_pos";
                negativeKey = "roll_neg";
            }
            case THRUST -> {
                positiveKey = "throttle";
                negativeKey = "brake";
            }
            default -> throw new IllegalStateException("Unexpected value: " + axis);
        }

        JsonObject positive = legacyDirectionObject(legacyAxes, positiveKey);
        JsonObject negative = legacyDirectionObject(legacyAxes, negativeKey);
        if (positive == null && negative == null) {
            return null;
        }

        return new AxisBindings(
                positive == null ? DirectionBinding.unbound() : DirectionBinding.fromLegacyJson(positive),
                negative == null ? DirectionBinding.unbound() : DirectionBinding.fromLegacyJson(negative),
                DEFAULT_SENSITIVITY
        );
    }

    private static JsonObject legacyDirectionObject(JsonObject axes, String key) {
        if (!axes.has(key) || !axes.get(key).isJsonObject()) {
            return null;
        }
        return axes.getAsJsonObject(key);
    }

    public enum AxisDirection {
        YAW_POS(ControlChairAxis.YAW, true, "yaw_pos", "Yaw +"),
        YAW_NEG(ControlChairAxis.YAW, false, "yaw_neg", "Yaw -"),
        PITCH_POS(ControlChairAxis.PITCH, true, "pitch_pos", "Pitch +"),
        PITCH_NEG(ControlChairAxis.PITCH, false, "pitch_neg", "Pitch -"),
        ROLL_POS(ControlChairAxis.ROLL, true, "roll_pos", "Roll +"),
        ROLL_NEG(ControlChairAxis.ROLL, false, "roll_neg", "Roll -"),
        THRUST_POS(ControlChairAxis.THRUST, true, "thrust_pos", "Thrust +"),
        THRUST_NEG(ControlChairAxis.THRUST, false, "thrust_neg", "Thrust -");

        public static final AxisDirection[] VALUES = values();

        private final ControlChairAxis axis;
        private final boolean positive;
        private final String serializedName;
        private final String displayName;

        AxisDirection(ControlChairAxis axis, boolean positive, String serializedName, String displayName) {
            this.axis = axis;
            this.positive = positive;
            this.serializedName = serializedName;
            this.displayName = displayName;
        }

        public ControlChairAxis axis() {
            return axis;
        }

        public boolean positive() {
            return positive;
        }

        public String serializedName() {
            return serializedName;
        }

        public String displayName() {
            return displayName;
        }
    }

    public enum AnalogBindingType {
        NONE,
        GAMEPAD_AXIS_POSITIVE,
        GAMEPAD_AXIS_NEGATIVE,
        MOUSE_YAW_POSITIVE,
        MOUSE_YAW_NEGATIVE,
        MOUSE_PITCH_POSITIVE,
        MOUSE_PITCH_NEGATIVE
    }

    public enum DigitalBindingType {
        NONE,
        KEYBOARD_KEY,
        MOUSE_BUTTON,
        GAMEPAD_BUTTON
    }

    public record AnalogBinding(AnalogBindingType type, int code) {
        public static AnalogBinding unbound() {
            return new AnalogBinding(AnalogBindingType.NONE, -1);
        }

        public static AnalogBinding gamepadAxisPositive(int axis) {
            return new AnalogBinding(AnalogBindingType.GAMEPAD_AXIS_POSITIVE, axis);
        }

        public static AnalogBinding gamepadAxisNegative(int axis) {
            return new AnalogBinding(AnalogBindingType.GAMEPAD_AXIS_NEGATIVE, axis);
        }

        public static AnalogBinding mouseYawPositive() {
            return new AnalogBinding(AnalogBindingType.MOUSE_YAW_POSITIVE, -1);
        }

        public static AnalogBinding mouseYawNegative() {
            return new AnalogBinding(AnalogBindingType.MOUSE_YAW_NEGATIVE, -1);
        }

        public static AnalogBinding mousePitchPositive() {
            return new AnalogBinding(AnalogBindingType.MOUSE_PITCH_POSITIVE, -1);
        }

        public static AnalogBinding mousePitchNegative() {
            return new AnalogBinding(AnalogBindingType.MOUSE_PITCH_NEGATIVE, -1);
        }

        public boolean isBound() {
            return type != AnalogBindingType.NONE;
        }

        public JsonObject toJson() {
            JsonObject object = new JsonObject();
            object.addProperty("type", type.name());
            object.addProperty("code", code);
            return object;
        }

        public static AnalogBinding fromJson(JsonObject object) {
            AnalogBindingType type = object.has("type")
                    ? safeEnum(AnalogBindingType.class, object.get("type").getAsString(), AnalogBindingType.NONE)
                    : AnalogBindingType.NONE;
            int code = object.has("code") ? object.get("code").getAsInt() : -1;
            return new AnalogBinding(type, code);
        }
    }

    public record DigitalBinding(DigitalBindingType type, int code) {
        public static DigitalBinding unbound() {
            return new DigitalBinding(DigitalBindingType.NONE, -1);
        }

        public static DigitalBinding key(int key) {
            return new DigitalBinding(DigitalBindingType.KEYBOARD_KEY, key);
        }

        public static DigitalBinding mouse(int button) {
            return new DigitalBinding(DigitalBindingType.MOUSE_BUTTON, button);
        }

        public static DigitalBinding gamepad(int button) {
            return new DigitalBinding(DigitalBindingType.GAMEPAD_BUTTON, button);
        }

        public boolean isBound() {
            return type != DigitalBindingType.NONE;
        }

        public JsonObject toJson() {
            JsonObject object = new JsonObject();
            object.addProperty("type", type.name());
            object.addProperty("code", code);
            return object;
        }

        public static DigitalBinding fromJson(JsonObject object) {
            DigitalBindingType type = object.has("type")
                    ? safeEnum(DigitalBindingType.class, object.get("type").getAsString(), DigitalBindingType.NONE)
                    : DigitalBindingType.NONE;
            int code = object.has("code") ? object.get("code").getAsInt() : -1;
            return new DigitalBinding(type, code);
        }
    }

    public record DirectionBinding(AnalogBinding analogBinding, DigitalBinding buttonBinding) {
        public static DirectionBinding unbound() {
            return new DirectionBinding(AnalogBinding.unbound(), DigitalBinding.unbound());
        }

        public DirectionBinding copy() {
            return new DirectionBinding(analogBinding, buttonBinding);
        }

        public DirectionBinding withAnalogBinding(AnalogBinding binding) {
            return new DirectionBinding(binding, buttonBinding);
        }

        public DirectionBinding withButtonBinding(DigitalBinding binding) {
            return new DirectionBinding(analogBinding, binding);
        }

        public JsonObject toJson() {
            JsonObject object = new JsonObject();
            object.add("analog", analogBinding.toJson());
            object.add("button", buttonBinding.toJson());
            return object;
        }

        public static DirectionBinding fromJson(JsonObject object) {
            AnalogBinding analog = object.has("analog") && object.get("analog").isJsonObject()
                    ? AnalogBinding.fromJson(object.getAsJsonObject("analog"))
                    : AnalogBinding.unbound();
            DigitalBinding button = object.has("button") && object.get("button").isJsonObject()
                    ? DigitalBinding.fromJson(object.getAsJsonObject("button"))
                    : DigitalBinding.unbound();
            return new DirectionBinding(analog, button);
        }

        public static DirectionBinding fromLegacyJson(JsonObject object) {
            return fromJson(object);
        }
    }

    public record AxisBindings(DirectionBinding positive, DirectionBinding negative, double sensitivity) {
        public AxisBindings {
            sensitivity = clampSensitivity(sensitivity);
        }

        public static AxisBindings unbound() {
            return new AxisBindings(DirectionBinding.unbound(), DirectionBinding.unbound(), DEFAULT_SENSITIVITY);
        }

        public AxisBindings copy() {
            return new AxisBindings(positive.copy(), negative.copy(), sensitivity);
        }

        public AxisBindings withPositive(DirectionBinding binding) {
            return new AxisBindings(binding, negative, sensitivity);
        }

        public AxisBindings withNegative(DirectionBinding binding) {
            return new AxisBindings(positive, binding, sensitivity);
        }

        public AxisBindings withSensitivity(double sensitivity) {
            return new AxisBindings(positive, negative, sensitivity);
        }

        public JsonObject toJson() {
            JsonObject object = new JsonObject();
            object.add("positive", positive.toJson());
            object.add("negative", negative.toJson());
            object.addProperty("sensitivity", sensitivity);
            return object;
        }

        public static AxisBindings fromJson(JsonObject object) {
            DirectionBinding positive = object.has("positive") && object.get("positive").isJsonObject()
                    ? DirectionBinding.fromJson(object.getAsJsonObject("positive"))
                    : DirectionBinding.unbound();
            DirectionBinding negative = object.has("negative") && object.get("negative").isJsonObject()
                    ? DirectionBinding.fromJson(object.getAsJsonObject("negative"))
                    : DirectionBinding.unbound();
            double sensitivity = object.has("sensitivity") ? object.get("sensitivity").getAsDouble() : DEFAULT_SENSITIVITY;
            return new AxisBindings(positive, negative, sensitivity);
        }
    }

    public record ButtonBindings(DigitalBinding primary, DigitalBinding secondary) {
        public static ButtonBindings unbound() {
            return new ButtonBindings(DigitalBinding.unbound(), DigitalBinding.unbound());
        }

        public ButtonBindings copy() {
            return new ButtonBindings(primary, secondary);
        }

        public JsonObject toJson() {
            JsonObject object = new JsonObject();
            object.add("primary", primary.toJson());
            object.add("secondary", secondary.toJson());
            return object;
        }

        public static ButtonBindings fromJson(JsonObject object) {
            DigitalBinding primary = object.has("primary") && object.get("primary").isJsonObject()
                    ? DigitalBinding.fromJson(object.getAsJsonObject("primary"))
                    : DigitalBinding.unbound();
            DigitalBinding secondary = object.has("secondary") && object.get("secondary").isJsonObject()
                    ? DigitalBinding.fromJson(object.getAsJsonObject("secondary"))
                    : DigitalBinding.unbound();
            return new ButtonBindings(primary, secondary);
        }
    }

    public static double clampSensitivity(double sensitivity) {
        return Math.max(MIN_SENSITIVITY, Math.min(MAX_SENSITIVITY, sensitivity));
    }

    private static <T extends Enum<T>> T safeEnum(Class<T> clazz, String name, T fallback) {
        try {
            return Enum.valueOf(clazz, name);
        } catch (IllegalArgumentException ignored) {
            return fallback;
        }
    }
}
