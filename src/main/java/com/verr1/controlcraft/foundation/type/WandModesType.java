package com.verr1.controlcraft.foundation.type;

import com.verr1.controlcraft.content.gui.wand.mode.*;
import com.verr1.controlcraft.content.gui.wand.mode.base.WandAbstractMultipleSelectionMode;
import com.verr1.controlcraft.foundation.api.IWandMode;
import net.minecraft.network.chat.Component;

import java.util.HashMap;

public enum WandModesType {


    HINGE,
    SERVO,
    JOINT,
    DESTROY,
    DESTROY_ALL,
    SLIDER,
    LINK,
    DELINK,
    COMPILE
    ;


    public static final HashMap<WandModesType, IWandMode> modeOf = new HashMap<>();

    public String langKey = "";

    static {
        WandServoAssembleMode.createInstance();
        WandJointAssembleMode.createInstance();
        WandSliderAssembleMode.createInstance();
        WandJointConnectionMode.createInstance();
        WandDestroyConstrainMode.createInstance();
        WandDestroyAllConstrainMode.createInstance();
        WandLinkMode.createInstance();
        WandDelinkMode.createInstance();
        WandCompileMode.createInstance();
        modeOf.put(HINGE, WandJointConnectionMode.instance);
        modeOf.put(DESTROY, WandDestroyConstrainMode.instance);
        modeOf.put(DESTROY_ALL, WandDestroyAllConstrainMode.instance);
        modeOf.put(SERVO, WandServoAssembleMode.instance);
        modeOf.put(JOINT, WandJointAssembleMode.instance);
        modeOf.put(SLIDER, WandSliderAssembleMode.instance);
        modeOf.put(LINK, WandLinkMode.instance);
        modeOf.put(DELINK, WandDelinkMode.instance);
        modeOf.put(COMPILE, WandCompileMode.instance);
    };


    WandModesType(){
        this.langKey = name().toLowerCase();
    }


    public static IWandMode modeOf(WandModesType type){
        return modeOf.get(type);
    }



    public Component tickCallBackInfo(WandAbstractMultipleSelectionMode.State state) {
        return Component.translatable("wand.mode.callback." + state.name().toLowerCase() + "." + langKey);
    }

}
