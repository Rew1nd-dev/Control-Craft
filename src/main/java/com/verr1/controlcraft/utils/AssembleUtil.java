package com.verr1.controlcraft.utils;

import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.mod.common.assembly.ShipAssembler;

import java.util.Set;

public class AssembleUtil {

    public static @Nullable ServerShip assembleToShip(ServerLevel level, Set<BlockPos> collected, double scale){
        try{
            return ShipAssembler.assembleToShip(level, collected, scale);
        }catch (Exception e){
            return null;
        }
    }

}
