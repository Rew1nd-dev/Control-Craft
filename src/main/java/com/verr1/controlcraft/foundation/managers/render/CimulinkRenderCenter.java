package com.verr1.controlcraft.foundation.managers.render;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.content.links.CimulinkRenderer;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.links.ClientViewContext;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nullable;


@OnlyIn(Dist.CLIENT)
public class CimulinkRenderCenter {




    // given a cbe to check and a viewHitVec, return the closest looking port pos and name index
    public static @Nullable ClientViewContext computeContext(
            @NotNull BlockPos cbePos,
            @NotNull Vec3 viewHitVec,
            @NotNull Level world
    ){
        CimulinkBlockEntity<?> cbe =
                BlockEntityGetter.getLevelBlockEntityAt(world, cbePos, CimulinkBlockEntity.class)
                .orElse(null);
        if(cbe == null)return null;
        return ((CimulinkRenderer)cbe.renderer()).computeContext(viewHitVec, true);
    }

    public static @Nullable ClientViewContext computeContextUntransformed(
            @NotNull BlockPos cbePos,
            @NotNull Vec3 viewHitVec,
            @NotNull Level world
    ){
        CimulinkBlockEntity<?> cbe =
                BlockEntityGetter.getLevelBlockEntityAt(world, cbePos, CimulinkBlockEntity.class)
                        .orElse(null);
        if(cbe == null)return null;
        return ((CimulinkRenderer)cbe.renderer()).computeContext(viewHitVec, false);
    }


    public record ComputeContext(int id, String portName, BlockPos pos, Vec3 portPos, double result, boolean isInput){}





    public static @Nullable CimulinkBlockEntity<?> of(BlockPos clientPos){

        return (CimulinkBlockEntity<?>) BlockEntityGetter.getLevelBlockEntityAt(
                        Minecraft.getInstance().level,
                        clientPos,
                        CimulinkBlockEntity.class
                )
                .orElse(null);
    }



}
