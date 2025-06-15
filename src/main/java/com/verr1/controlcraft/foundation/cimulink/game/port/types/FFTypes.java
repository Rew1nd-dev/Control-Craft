package com.verr1.controlcraft.foundation.cimulink.game.port.types;

import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Temporal;
import com.verr1.controlcraft.foundation.cimulink.game.ComponentInstances;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;
import net.minecraft.util.StringRepresentable;
import org.jetbrains.annotations.NotNull;

import java.util.List;

import static com.verr1.controlcraft.utils.ComponentUtils.literals;

public enum FFTypes implements Inspectable<NamedComponent>, Descriptive<FFTypes>, StringRepresentable {
    T_FF(ComponentInstances.T_FF, literals("1->1 T Flip-Flop")),
    D_FF(ComponentInstances.D_FF, literals("1->1 D Flip-Flop")),
    JK_FF(ComponentInstances.JK_FF, literals("2->2 JK Flip-Flop")),
    RS_FF(ComponentInstances.RS_FF, literals("2->2 RS Flip-Flop")),
    ASYNC_T_FF(ComponentInstances.ASYNC_T_FF, literals("1->1 T Flip-Flop With explicit clk Port")),
    ASYNC_D_FF(ComponentInstances.ASYNC_D_FF, literals("1->1 D Flip-Flop With explicit clk Port")),
    ASYNC_JK_FF(ComponentInstances.ASYNC_JK_FF, literals("2->2 JK Flip-Flop With explicit clk Port")),
    ASYNC_RS_FF(ComponentInstances.ASYNC_RS_FF, literals("2->2 RS Flip-Flop With explicit clk Port"));;

    private final ComponentInstances.Inspector<NamedComponent> inspector;

    FFTypes(
            ComponentInstances.Inspector<NamedComponent> inspector,
            List<Component> description

    ) {
        this.inspector = inspector;
        LangUtils.registerDefaultName(FFTypes.class, this, Component.literal(name().toUpperCase()));
        LangUtils.registerDefaultDescription(FFTypes.class, this, description);
    }

    @Override
    public ComponentInstances.Inspector<NamedComponent> inspector() {
        return inspector;
    }

    @Override
    public FFTypes self() {
        return this;
    }

    @Override
    public Class<FFTypes> clazz() {
        return FFTypes.class;
    }

    public static void register(){

    }

    @Override
    public @NotNull String getSerializedName() {
        return name().toLowerCase();
    }
}
