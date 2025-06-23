package com.verr1.controlcraft.content.compact.createbigcannons.impl;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.compact.createbigcannons.ICannonMountPeripheralGetter;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import dan200.computercraft.api.peripheral.IPeripheral;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import rbasamoyai.createbigcannons.cannon_control.cannon_mount.CannonMountBlockEntity;

import java.util.concurrent.atomic.AtomicBoolean;

public class CannonMountPeripheralGetter implements ICannonMountPeripheralGetter {
    @Override
    public IPeripheral get(ServerLevel level, BlockPos pos) {
        AtomicBoolean found = new AtomicBoolean(false);
        IPeripheral ip = BlockEntityGetter.getLevelBlockEntityAt(level, pos, CannonMountBlockEntity.class)
                .map(be -> {
                    found.set(true);
                    return new CannonMountPeripheral(be);
                })
                .orElse(null);

        if(ip == null && found.get()){
            ControlCraft.LOGGER.info("cannon mount peripheral getter failed at pos: {}", pos);
        }
        return ip;
    }
}
