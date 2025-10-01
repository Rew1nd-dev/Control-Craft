package com.verr1.controlcraft.foundation.cimulink.game;

import com.simibubi.create.content.kinetics.speedController.SpeedControllerBlockEntity;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.content.compact.tweak.TweakControllerCompact;
import com.verr1.controlcraft.content.compact.vssw.VSSWCompact;
import com.verr1.controlcraft.content.links.proxy.ProxyLinkBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.SpeedControllerPlant;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntity;
import org.jetbrains.annotations.Nullable;

public class PlantGetter {

    public static @Nullable NamedComponent get(ServerLevel level, BlockPos pos){
        BlockEntity be = BlockEntityGetter.getLevelBlockEntityAt(
                        level,
                        pos,
                        BlockEntity.class
                )
                .orElse(null);

        if (be instanceof IPlant iPlant && !(be instanceof ProxyLinkBlockEntity)){
            return iPlant.plant();
        }
        if (be instanceof SpeedControllerBlockEntity sp){
            return new SpeedControllerPlant(sp);
        }

        NamedComponent tweak = TweakControllerCompact.tweakedControllerPlant(level, pos);
        if(tweak != null)return tweak;

        NamedComponent cannon = CreateBigCannonsCompact.cannonMountPlant(level, pos);
        if(cannon != null)return cannon;

        NamedComponent seat = VSSWCompact.vsswSeatPlant(level, pos);
        if(seat != null)return seat;

        return null;
    }

}
