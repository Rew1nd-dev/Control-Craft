package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.ad.Comparator;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ICompilable;
import com.verr1.controlcraft.foundation.cimulink.game.port.ISummarizable;
import com.verr1.controlcraft.foundation.cimulink.game.registry.CimulinkFactory;

public class ComparatorLinkPort extends BlockLinkPort implements ICompilable<Comparator> {

    public ComparatorLinkPort() {
        super(new Comparator());
    }

    @Override
    public NamedComponent create() {
        return new Comparator();
    }


    @Override
    public Comparator component() {
        return (Comparator) __raw();
    }

    @Override
    public CimulinkFactory.Factory<Comparator> factory() {
        return CimulinkFactory.COMPARATOR;
    }


}
