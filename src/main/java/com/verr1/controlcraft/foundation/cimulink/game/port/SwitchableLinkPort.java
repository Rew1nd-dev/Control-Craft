package com.verr1.controlcraft.foundation.cimulink.game.port;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.Inspectable;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;

public class SwitchableLinkPort<T extends Inspectable<?>> extends BlockLinkPort {

    private T currentType;

    protected SwitchableLinkPort(WorldBlockPos portPos, T defaultValue) {
        super(portPos, defaultValue.inspector().get());
    }


    public void setCurrentType(T type){
        if(type == currentType)return;
        currentType = type;
        recreate();
    }

    @Override
    public NamedComponent create() {
        return currentType.inspector().get();
    }
}
