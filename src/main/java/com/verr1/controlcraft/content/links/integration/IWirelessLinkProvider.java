package com.verr1.controlcraft.content.links.integration;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import net.minecraft.core.BlockPos;

public interface IWirelessLinkProvider {

    NamedComponent linkCircuit();

    boolean useDecimalNetwork();

    BlockPos getBlockPos();

    boolean isRemoved();

}
