package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.SwitchableLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;

public class FFLinkPort extends SwitchableLinkPort<FFTypes> {

    public FFLinkPort(WorldBlockPos portPos) {
        super(portPos, FFTypes.D_FF);
    }
}
