package com.verr1.controlcraft.foundation.type.descriptive;

import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.List;

import static com.verr1.controlcraft.utils.ComponentUtils.literals;

public enum UIContents implements Descriptive<UIContents> {
    CURRENT(Component.literal("Current"), literals("Current Angle, Velocity, Position Etc.")),

    TARGET_ANGLE(Component.literal("Angle"), literals("Target Angle")),
    TARGET_OMEGA(Component.literal("Omega"), literals("Target Velocity")),
    TARGET_DISTANCE(Component.literal("Distance"), literals("Target Distance")),
    TARGET_VELOCITY(Component.literal("Velocity"), literals("Target Velocity")),

    CURRENT_ANGLE(Component.literal("Angle"), literals("Current Angle")),
    CURRENT_OMEGA(Component.literal("Omega"), literals("Current Velocity")),
    CURRENT_DISTANCE(Component.literal("Distance"), literals("Current Distance")),
    CURRENT_VELOCITY(Component.literal("Velocity"), literals("Current Velocity")),

    LOCKED(Component.literal("Locked"), literals("Whether The Device Is Locked By Constraint")),
    TARGET(Component.literal("Target"), literals("Target Angle, Velocity, Position Etc.")),
    SELF_OFFSET(Component.literal("Offset"), literals("Rotation Axis Offset For Next Assembly / Connection")),
    COMP_OFFSET(Component.literal("Offset"), literals("Companion Offset For Next Assembly / Connection")),
    SPEED_LIMIT(Component.literal("Limit"), literals("Maximum Rotational Speed")),

    MODE(Component.literal("Mode"), literals("Velocity / Position")),
    CHEAT(Component.literal("Cheat"), literals("Convenience")),
    AUTO_LOCK(Component.literal("Auto Lock"), literals("Locked When:", " .Target Speed = 0", " .Target Angle Reached")),
    PID_CONTROLLER(Component.literal("PID Controller"), literals("Integrated Proportional Integral Derivative Controller")),
    QPID_CONTROLLER(Component.literal("Rotation"), literals("PID For Rotation")),
    PPID_CONTROLLER(Component.literal("Position"), literals("PID For Position")),

    COMPLIANCE(Component.literal("Compliance"), literals("actual value = 10 ^ (ui value)")),

    MIN(Component.literal("Min"), literals("Minimum Value Of Signal 0")),
    MAX(Component.literal("Max"), literals("Maximum Value Of Signal 15")),

    TYPE(Component.literal("Type"), literals("Type Of The Peripheral")),
    PROTOCOL(Component.literal("protocol"), literals("Unique Channel")),
    NAME(Component.literal("Name"), literals("Unique Name Under A Same protocol")),
    SPATIAL_OFFSET(Component.literal("Spatial Offset"), literals("Offset Distance In Space")),

    ANCHOR_RESISTANCE_AT_POS(Component.literal("Resist At Pos"), literals("Resistance Apply To Block Instead Of COM")),
    ANCHOR_EXTRA_GRAVITY_AT_POS(Component.literal("Gravity At Pos"), literals("Extra Gravity Apply To Block Instead Of COM")),
    ANCHOR_SQUARE_DRAG(Component.literal("Square Drag"), literals("Apply Square Drag Instead Of Linear")),

    CAMERA_LINK_ACCEPT(Component.literal("Camera Link"), literals("Link To Camera")),
    CAMERA_LINK_DUMP(Component.literal("Camera Dump"), literals("Dump Camera Link")),
    CAMERA_LINK_RESET(Component.literal("Camera Reset"), literals("Dump All Camera Link")),
    CAMERA_LINK_VALIDATE(Component.literal("Camera Validate"), literals("Dump Unloaded Or Removed Camera Link")),
    CAMERA_VIEW_RESET(Component.literal("Camera View Reset"), literals("Reset Camera Ray")),

    ASSEMBLY(Component.literal("Assembly"), literals("Assemble Contraption or Ship")),
    DISASSEMBLY(Component.literal("Dis Assembly"), literals("Disassemble Contraption or Ship")),
    LOCK(Component.literal("Lock"), literals("Lock The Device")),
    UNLOCK(Component.literal("Unlock"), literals("Unlock The Device")),

    FORCED(Component.literal("Forced"), literals("Force Online Mode", "Divert others if key is used", "Will try force online every 0.5s")),
    ONLINE(Component.literal("Online"), literals("Online Hold Key")),
    OFFLINE(Component.literal("Offline"), literals("Offline Hold Key")),


    FLAP_OFFSET(Component.literal("Offset"), literals("Angle Offset")),
    FLAP_LIFT(Component.literal("Lift"), literals("Lift Ratio")),
    FLAP_DRAG(Component.literal("Drag"), literals("Drag Ratio")),
    FLAP_BIAS(Component.literal("Bias"), literals("Attack Angle Bias")),

    GATE_TYPES(Component.literal("Type"), literals("Logic Gate Types")),
    FF_TYPES(Component.literal("Type"), literals("Flip Flop Types")),

    FUNCTIONS_TYPES(Component.literal("Type"), literals("Functions Types")),
    FUNCTIONS_GROUP(Component.literal("Group"), literals("Functions Group")),

    LINK_INPUT(Component.literal("Input"), literals("Input Port Value")),
    LINK_OUTPUT(Component.literal("Output"), literals("Output Port Value")),

    SHIFTER_DELAY(Component.literal("Delay"), literals("Shifter Delay")),
    SHIFTER_PARALLEL(Component.literal("Parallel"), literals("Shifter Inputs Size")),

    ASYNC_COMPONENT(Component.literal("Async"), literals("Has Explicit Clk Port")),

    FMA_COEFFICIENT(Component.literal("Coefficients"), literals("Linear Adder Coefficients")),

    FMA_INC(Component.literal("Add Input"), literals("Linear Adder Coefficients")),
    FMA_DEC(Component.literal("Del Input"), literals("Linear Adder Coefficients")),

    STATUS(Component.literal("Available Ports"), literals("Ports of this plant")),

    SENSOR_SETTINGS(Component.literal("Sensor Settings"), literals("Settings for the sensor")),
    SENSOR_TYPE(Component.literal("Type"), literals("Which metric to measure")),
    SENSOR_LOCAL(Component.literal("Local"), literals("Transform vector to local coordinate")),

    PLACE_HOLDER(Component.literal(""), literals(""))
    ;




    public FormattedLabel toUILabel() {
        var l = new FormattedLabel(0, 0, asComponent());
        l.setText(asComponent());
        return l;
    }

    public Component title(){
        return this.asComponent().plainCopy().withStyle(Converter::titleStyle);
    }

    public Component option(){
        return this.asComponent().plainCopy().withStyle(Converter::optionStyle);
    }

    UIContents(Component displayName, List<Component> description) {
        LangUtils.registerDefaultName(UIContents.class, this, displayName);
        LangUtils.registerDefaultDescription(UIContents.class, this, description);

    }

    @Override
    public UIContents self() {
        return this;
    }

    @Override
    public Class<UIContents> clazz() {
        return UIContents.class;
    }

    public static void register(){
        LangUtils.registerDefaultDescription(UIContents.class, literals("UI Contents"));
    }
}
