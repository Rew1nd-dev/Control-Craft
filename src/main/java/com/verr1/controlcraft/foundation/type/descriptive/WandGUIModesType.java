package com.verr1.controlcraft.foundation.type.descriptive;

import com.simibubi.create.foundation.gui.AllIcons;
import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.Arrays;
import java.util.List;

import static com.verr1.controlcraft.utils.ComponentUtils.literals;

public enum WandGUIModesType implements Descriptive<WandGUIModesType> {
    CONNECT(literals(
            "Connect Ships To Device",
            "Currently Applicable:| Motors | Piston | Joints | Cimulink")),
    DISCONNECT(literals(
            "Disconnect Ships From Device",
            "Currently Applicable:| Motors | Piston | Joints | Cimulink",
            "Right Click Device To Start",
            "Right Click Companion Ship Block Face(s) to Select the Face to Connect",
            "Right Click Air to Confirm, Shift + Right Click to Restart"
    )),
    DISCONNECT_ALL(literals(
            "Click Ship To Apply, Destroy All Constraints",
            "Won't Reset Device, Need Manually Replace",
            "Don't Use This Unless There's Some ",
            "Constraint Cannot Be Removed Normally"
            )),
    COMPILE(literals(
            "Select 2 Diagonal Corners To Select An Area",
            "Right Click Air To Confirm, Creates a packaged subsystem",
            "Input Port And Output Port Will Be Convert to Subsystem io port",
            "Uncompilable:| Proxy | Sensor |"
    ))
    ;

    @Override
    public WandGUIModesType self() {
        return this;
    }

    @Override
    public Class<WandGUIModesType> clazz() {
        return WandGUIModesType.class;
    }

    WandGUIModesType(List<Component> description){
        LangUtils.registerDefaultName(WandGUIModesType.class, this, Component.literal(name()));
        LangUtils.registerDefaultDescription(WandGUIModesType.class, this, description);
    }


    public AllIcons getIcon(){
        return AllIcons.I_TOOLBOX;
    }

    public static List<WandGUIModesType> getAllTypes(){
        return Arrays.stream(WandGUIModesType.values()).toList();
    }
    public static void register(){
        // load by class loader and constructors will call registerDefaultName etc
    }

}
