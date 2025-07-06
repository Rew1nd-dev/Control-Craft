package com.verr1.controlcraft.foundation.redstone;

import com.simibubi.create.content.redstone.link.IRedstoneLinkable;

public interface $IRedstoneLinkable extends IRedstoneLinkable {

    void $setReceivedStrength(double decimal);

    double $getTransmittedStrength();

    boolean isSource();
}
