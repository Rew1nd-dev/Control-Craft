package com.verr1.controlcraft.content.compact.vssw.impl;

import com.verr1.controlcraft.content.compact.vssw.IVSSWCompactProxy;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.shao.valkyrien_space_war.block.seat.base.BaseShipControlSeatBE;

public class VSSWCompactProxy implements IVSSWCompactProxy {
    @Override
    public NamedComponent getVSSWSeatPlant(ServerLevel level, BlockPos pos) {
        return BlockEntityGetter.getLevelBlockEntityAt(level, pos, BaseShipControlSeatBE.class)
                .map(ControlSeatPlant::new)
                .orElse(null);
    }
}
