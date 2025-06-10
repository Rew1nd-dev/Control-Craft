package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.game.port.SwitchableLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;

public class FFLinkPort extends SwitchableLinkPort<FFTypes> {

    public FFLinkPort() {
        super(FFTypes.D_FF);
    }

    @Override
    protected Class<FFTypes> clazz() {
        return FFTypes.class;
    }
}
