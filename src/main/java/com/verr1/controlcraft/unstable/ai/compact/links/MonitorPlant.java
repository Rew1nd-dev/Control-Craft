package com.verr1.controlcraft.unstable.ai.compact.links;

import com.verr1.controlcraft.foundation.cimulink.game.peripheral.Plant;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import com.verr1.controlcraft.unstable.blocks.monitor.MonitorBlockEntity;

public class MonitorPlant extends Plant {

    public MonitorPlant(MonitorBlockEntity cbe) {
        super(new builder());
    }
}
