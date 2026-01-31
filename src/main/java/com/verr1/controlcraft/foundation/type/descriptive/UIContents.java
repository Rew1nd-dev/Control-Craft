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
    COLLISION(Component.literal("Collision"), literals("Collision With Companion")),
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
    CAMERA_STAB(Component.literal("Stabilizer"), literals("")),
    CAMERA_3(Component.literal("F5 Stab"), literals("")),

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

    AVAILABLE_PORTS(Component.literal("Available Ports"), literals("Name With Ports of this ship")),

    AVAILABLE_IN_PORTS(Component.literal("Available In"), literals("Input Ports of this ship")),
    AVAILABLE_OUT_PORTS(Component.literal("Available Out"), literals("Output Ports of this ship")),

    ADD_PORT_TO_BUS(Component.literal("Add"), literals("Add This Port To Bus")),
    EXPOSED_PORTS(Component.literal("Exposed Ports"), literals("The ports that can be connected")),
    RET_PORT_FROM_BUS(Component.literal("Remove"), literals("Remove This Port From Bus")),

    SENSOR_SETTINGS(Component.literal("Sensor Settings"), literals("Settings for the sensor")),
    SENSOR_TYPE(Component.literal("Type"), literals("Which metric to measure")),
    SENSOR_LOCAL(Component.literal("Local"), literals("Transform vector to local coordinate")),

    USE_DECIMAL(
            Component.literal("Decimal"),
            literals(
                    "Use decimal Network to transmit wireless signal",
                    "Enabling this will stop circuit from transmit or receive from",
                    "regular create redstone network")
    ),

    MASK_IN(Component.literal("Negate Input"), literals("Negate Input Signal")),
    MASK_OUT(Component.literal("Negate Output"), literals("Negate Output Signal")),

    PLACE_HOLDER(Component.literal(""), literals("")),



    AI_FLIGHT(Component.literal("Actual Flight"), literals("Use Real Flight")),
    AI_WEAPON(Component.literal("Actual Weapon"), literals("Use Real Weapon")),
    AI_DEAD(Component.literal("Dead"), literals("Is Discarded")),

    AI_YAW(Component.literal("Yaw"), literals("Yaw Omega")),
    AI_VEL(Component.literal("Vel"), literals("Cruise Velocity")),
    AI_RAD(Component.literal("Radius"), literals("Cruise Radius")),
    AI_E_RAD(Component.literal("Ex-Radius"), literals("Cruise Radius")),
    AI_P_DRIVE(Component.literal("PDrive"), literals("Proportional Drive Coefficient")),
    AI_I_DRIVE(Component.literal("IDrive"), literals("Integral Drive Coefficient")),
    AI_TURN_RESIST(Component.literal("TurnResist"), literals("Turn Resistance Coefficient")),
    AI_FIRE_RATE(Component.literal("fire_rate"), literals("fire rate")),
    AI_DEATH_RATIO(Component.literal("Ratio"), literals("Discard When Achieve Mass Percentage")),
    AI_PROJ_VEL(Component.literal("Vel"), literals("Projectile Velocity m/t")),
    AI_DB_ARROW(Component.literal("shoot_arrow"), literals("no damage")),
    AI_TOL(Component.literal("Tol"), literals("Shoot Angle Tolerance")),
    AI_SPREAD(Component.literal("Spread"), literals("Spread")),
    AI_TAR(Component.literal("Target"), literals("Track A Spinalyzer With Peripheral Interface Named By This Setting")),
    AI_TWI(Component.literal("Twist"), literals("Roll Omega")),

    AI_STRIKE_D(Component.literal("StrikeD"), literals("Strike Distance")),
    AI_STRIKE_H(Component.literal("StrikeH"), literals("Strike End Height")),
    AI_ENTER_MIN(Component.literal("EnterMin"), literals("Enter Min Pitch")),
    AI_ENTER_MAX(Component.literal("EnterMax"), literals("Enter Max Pitch")),

    AI_P_COMMON(Component.literal("P"), literals("")),
    AI_P_YAW(Component.literal("PYaw"), literals("")),
    AI_P_PITCH(Component.literal("PPitch"), literals("")),
    AI_P_AG_ROLL(Component.literal("PAGRoll"), literals("")),
    AI_P_LV_ROLL(Component.literal("PLVRoll"), literals("")),

    AI_DEPLOY_ROT(Component.literal("Rot"), literals("")),
    AI_DEPLOY_POS(Component.literal("Pos"), literals("")),
    AI_LOCAL(Component.literal("Local"), literals("")),
    AI_INHERIT(Component.literal("Inherit"), literals("Inherit Base Velocity And Angular Velocity")),
    AI_DEPLOY(Component.literal("Deploy"), literals("Deploy This AI")),
    AI_DISTANCE(Component.literal("Distance"), literals("Projectile Spawn Distance")),


    AI_SCHEME_NAME(Component.literal("Name"), literals("Schematic Name")),
    AI_SCHEME_NAMESPACE(Component.literal("Namespace"), literals("Schematic Namespace")),
    AI_SCHEME_NAMESPACE2(Component.literal("Space"), literals("Schematic Namespace")),
    AI_EXPORT_SCHEME(Component.literal("Export"), literals("Export Schematic To ", ".minecraft/ai_schematics")),
    AI_DEPLOY_SCHEME(Component.literal("Deploy"), literals("Deploy Schematic Here")),
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
