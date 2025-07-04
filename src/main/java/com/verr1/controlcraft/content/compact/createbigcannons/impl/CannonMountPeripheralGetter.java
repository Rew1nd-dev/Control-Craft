package com.verr1.controlcraft.content.compact.createbigcannons.impl;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.compact.createbigcannons.ICannonMountPeripheralGetter;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.mixinducks.ICannonDuck;
import dan200.computercraft.api.peripheral.IPeripheral;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;
import rbasamoyai.createbigcannons.CreateBigCannons;
import rbasamoyai.createbigcannons.cannon_control.cannon_mount.CannonMountBlockEntity;
import rbasamoyai.createbigcannons.index.CBCBlocks;

import java.util.concurrent.atomic.AtomicBoolean;

public class CannonMountPeripheralGetter implements ICannonMountPeripheralGetter {
    @Override
    public IPeripheral getComputercraft(ServerLevel level, BlockPos pos) {
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

    @Override
    public @Nullable NamedComponent getCimulink(ServerLevel level, BlockPos pos) {
        return BlockEntityGetter.getLevelBlockEntityAt(level, pos, ICannonDuck.class)
                .map(CannonMountPlant::new)
                .orElse(null);
    }

    @Override
    public BlockState cannonMountBlock(int type) {
        return switch (type){
            case 0 -> CBCBlocks.CANNON_MOUNT.getDefaultState();
            case 1 -> CBCBlocks.FIXED_CANNON_MOUNT.getDefaultState();
            default -> null;
        };
    }
}
