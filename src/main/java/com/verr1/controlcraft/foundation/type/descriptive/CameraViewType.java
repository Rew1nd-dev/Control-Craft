package com.verr1.controlcraft.foundation.type.descriptive;

import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.utils.LangUtils;
import net.minecraft.network.chat.Component;

import java.util.List;

import static com.verr1.controlcraft.utils.ComponentUtils.literals;
import static net.minecraft.network.chat.Component.literal;

public enum CameraViewType implements Descriptive<CameraViewType> {
    ROT(
            literal("Local"),
            literals("View Rotates With Ship")
    ),
    STAB(
            literal("Absolute"),
            literals("Independent View, Does Not Rotate With Ship")
    ),
    F5(
            literal("Absolute F5"),
            literals("Third Person Independent View")
    )
    ;


    CameraViewType(
            Component name,
            List<Component> description
    ){
        LangUtils.registerDefaultName(clazz(), self(), name);
        LangUtils.registerDefaultDescription(clazz(), self(), description);
    }

    @Override
    public CameraViewType self() {
        return this;
    }

    @Override
    public Class<CameraViewType> clazz() {
        return CameraViewType.class;
    }

    public static void register(){
        LangUtils.registerDefaultDescription(CameraClipType.class, literals("Camera View Settings"));
        // load by class loader and constructors will call registerDefaultName etc
    }

}
