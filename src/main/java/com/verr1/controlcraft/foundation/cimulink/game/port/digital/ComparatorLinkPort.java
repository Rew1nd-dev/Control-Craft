package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.ad.Comparator;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;

public class ComparatorLinkPort extends BlockLinkPort {

    public ComparatorLinkPort() {
        super(new Comparator());
    }

    @Override
    public NamedComponent create() {
        return new Comparator();
    }
}
