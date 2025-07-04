package com.verr1.controlcraft.content.compact.createbigcannons;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import dan200.computercraft.api.peripheral.IPeripheral;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.state.BlockState;

public interface ICannonMountPeripheralGetter {

    IPeripheral getComputercraft(ServerLevel level, BlockPos pos);

    NamedComponent getCimulink(ServerLevel level, BlockPos pos);

    BlockState cannonMountBlock(int type);

}
