package com.verr1.controlcraft.content.blocks.receiver;

import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import dan200.computercraft.shared.peripheral.generic.GenericPeripheral;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.impl.Peripherals;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.Nullable;

public class PeripheralGetter {

    public static @Nullable IPeripheral get(ServerLevel level, BlockPos pos, Direction dir){
        IPeripheral general = Peripherals.getPeripheral(
                level,
                pos,
                dir,
                () -> {}
        );
        IPeripheral compact = CreateBigCannonsCompact.cannonMountPeripheral(level, pos);
        if(compact == null)return general;
        return compact;
    }
}
