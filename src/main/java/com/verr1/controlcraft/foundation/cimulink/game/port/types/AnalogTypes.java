package com.verr1.controlcraft.foundation.cimulink.game.port.types;

import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Functions;
import com.verr1.controlcraft.foundation.cimulink.core.components.vectors.*;
import com.verr1.controlcraft.foundation.cimulink.game.ComponentInstances;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.List;

import static com.verr1.controlcraft.utils.ComponentUtils.literals;

public enum AnalogTypes implements
        Inspectable<NamedComponent>, Descriptive<AnalogTypes>, GroupEnum<AnalogGroups>
{



    MIN(ComponentInstances.MIN, literals("Output the minimum value of 2 inputs"), AnalogGroups.Basic),
    MAX(ComponentInstances.MAX, literals("Output the maximum value of 2 inputs"), AnalogGroups.Basic),
    PRODUCT(ComponentInstances.PRODUCT, literals("Output the product of 2 inputs"), AnalogGroups.Basic),
    DIV(ComponentInstances.DIV, literals("Output input 0 divided by input 1"), AnalogGroups.Basic),
    POWER(ComponentInstances.POWER, literals("Output i_0 raised to the power of i_1"), AnalogGroups.Basic),
    LOGARITHM(ComponentInstances.LOGARITHM, literals("Output ln(i1) / ln(i0)"), AnalogGroups.Basic),
    ABS(ComponentInstances.ABS, literals("Output the absolute value of the input"), AnalogGroups.Basic),

    ANGLE_FIX(ComponentInstances.ANGLE_FIX, literals("Coerce the input into (-pi, pi)"), AnalogGroups.Trigonometric),
    RAD(ComponentInstances.Inspector.of(Functions.RAD::get),
            List.of(Component.literal("Convert the input from degrees to radians")),
            AnalogGroups.Trigonometric
    ),
    DEG(ComponentInstances.Inspector.of(Functions.DEG::get),
            List.of(Component.literal("Convert the input from radians to degrees")),
            AnalogGroups.Trigonometric
    ),
    SIN(ComponentInstances.SIN, literals("Output the sine of the input"), AnalogGroups.Trigonometric),
    COS(ComponentInstances.COS, literals("Output the cosine of the input"), AnalogGroups.Trigonometric),
    TAN(ComponentInstances.TAN, literals("Output the tangent of the input"), AnalogGroups.Trigonometric),
    ASIN(
            ComponentInstances.Inspector.of(Functions.ASIN::get),
            List.of(Component.literal("Output the arc sine of the input, clamped to [-pi/2, pi/2]")),
            AnalogGroups.Trigonometric
    ),
    ACOS(
            ComponentInstances.Inspector.of(Functions.ACOS::get),
            List.of(Component.literal("Output the arc cosine of the input, clamped to [0, pi]")),
            AnalogGroups.Trigonometric
    ),
    ATAN(
            ComponentInstances.Inspector.of(Functions.ATAN::get),
            List.of(Component.literal("Output the arc tangent of the input, clamped to [-pi/2, pi/2]")),
            AnalogGroups.Trigonometric
    ),


    DOT(
            ComponentInstances.Inspector.of(Dot::new),
            List.of(Component.literal("Output the dot product of two vectors")),
            AnalogGroups.Vector
    ),

    CROSS(
            ComponentInstances.Inspector.of(Cross::new),
            List.of(Component.literal("Output the cross product of two vectors")),
            AnalogGroups.Vector
    ),

    Q_TRANSFORM(
            ComponentInstances.Inspector.of(QTransform::new),
            List.of(Component.literal("Output the quaternion transformation of a vector")),
            AnalogGroups.Vector
    ),

    V_NORM(
            ComponentInstances.Inspector.of(SafeNorm::new),
            List.of(Component.literal("Output the norm of a vector (length)")),
            AnalogGroups.Vector
    ),

    V_MAG(
            ComponentInstances.Inspector.of(Magnitude::new),
            List.of(Component.literal("Output the magnitude of a vector (length)")),
            AnalogGroups.Vector
    ),

    V_LOOK_ALONG(
            ComponentInstances.Inspector.of(LookAlong::new),
            List.of(Component.literal("See org.joml.Quaterniondc#lookAlong for more information")),
            AnalogGroups.Vector
    ),

    Q_SLERP(
            ComponentInstances.Inspector.of(Slerp::new),
            List.of(Component.literal("Output the spherical linear interpolation of two quaternions")),
            AnalogGroups.Vector
    ),

    Q_MUL(
            ComponentInstances.Inspector.of(QMul::new),
            List.of(Component.literal("Output the multiplication of two quaternions")),
            AnalogGroups.Vector
    )

    ;



    public static final AnalogTypes[] BASIC = new AnalogTypes[]{
            AnalogTypes.MIN, AnalogTypes.MAX, AnalogTypes.PRODUCT,
            AnalogTypes.DIV, AnalogTypes.POWER, AnalogTypes.LOGARITHM,
            AnalogTypes.ABS
    };
    public static final AnalogTypes[] TRIGONOMETRIC = new AnalogTypes[]{
            AnalogTypes.ANGLE_FIX, AnalogTypes.RAD, AnalogTypes.DEG,
            AnalogTypes.SIN, AnalogTypes.COS, AnalogTypes.TAN,
            AnalogTypes.ASIN, AnalogTypes.ACOS, AnalogTypes.ATAN
    };
    public static final AnalogTypes[] VECTOR = new AnalogTypes[]{
            AnalogTypes.DOT, AnalogTypes.CROSS, AnalogTypes.Q_TRANSFORM,
            AnalogTypes.V_NORM, AnalogTypes.V_MAG, AnalogTypes.V_LOOK_ALONG,
            AnalogTypes.Q_SLERP, AnalogTypes.Q_MUL
    };



    private final AnalogGroups subType;
    private final ComponentInstances.Inspector<NamedComponent> inspector;

    AnalogTypes(
            ComponentInstances.Inspector<NamedComponent> inspector,
            List<Component> description,
            AnalogGroups subType
    ) {
        this.inspector = inspector;
        this.subType = subType;
        LangUtils.registerDefaultName(clazz(), this, Component.literal(name().toUpperCase()));
        LangUtils.registerDefaultDescription(clazz(), this, description);
    }

    @Override
    public AnalogTypes self() {
        return this;
    }

    @Override
    public Class<AnalogTypes> clazz() {
        return AnalogTypes.class;
    }

    @Override
    public ComponentInstances.Inspector<NamedComponent> inspector() {
        return inspector;
    }

    public static void register(){
        AnalogGroups.register();
    }

    @Override
    public AnalogGroups group() {
        return subType;
    }


}
