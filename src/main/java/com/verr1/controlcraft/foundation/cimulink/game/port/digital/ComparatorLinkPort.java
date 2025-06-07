package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.ad.Comparator;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;

public class ComparatorLinkPort extends BlockLinkPort {

    public ComparatorLinkPort(WorldBlockPos portPos) {
        super(portPos, new Comparator());
    }

    @Override
    public NamedComponent create() {
        return new Comparator();
    }
}
