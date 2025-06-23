package com.verr1.controlcraft.content.compact.createbigcannons;

import dan200.computercraft.api.peripheral.IPeripheral;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;

public interface ICannonMountPeripheralGetter {

    IPeripheral get(ServerLevel level, BlockPos pos);

}
