package com.verr1.controlcraft.foundation.managers;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.links.ClientViewContext;
import com.verr1.controlcraft.foundation.data.links.ConnectionStatus;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nullable;

@OnlyIn(Dist.CLIENT)
public class CimulinkRenderCenter {
    static final double DELTA_Y = 0.15;

    public static Vec3 computeOutputPortOffset(Direction horizontal, Direction vertical, int count, int total){
        double x = 0.25;
        double y = (total - 1) * DELTA_Y / 2 - (count * DELTA_Y / 2);
        Vec3 h = MinecraftUtils.toVec3(horizontal.getNormal());
        Vec3 v = MinecraftUtils.toVec3(vertical.getNormal());
        return h.scale(x).add(v.scale(y));
    }

    public static Vec3 computeInputPortOffset(Direction horizontal, Direction vertical, int count, int total){
        double x = -0.25;
        double y = (total - 1) * DELTA_Y / 2 - (count * DELTA_Y / 2);
        Vec3 h = MinecraftUtils.toVec3(horizontal.getNormal());
        Vec3 v = MinecraftUtils.toVec3(vertical.getNormal());
        return h.scale(x).add(v.scale(y));
    }

    public static @Nullable ComputeContext closestInput(ConnectionStatus cs, Vec3 viewHitVec, Direction horizontal, Direction vertical, Vec3 blockCenter){
        int closestIndex = -1;
        double closestDistance = Double.MAX_VALUE;
        Vec3 closestVec = null;

        for(int i = 0; i < cs.inputs.size(); i++){
            Vec3 offset = computeInputPortOffset(horizontal, vertical, i, cs.inputs.size());
            Vec3 pos = blockCenter.add(offset);
            double distance = pos.distanceToSqr(viewHitVec);
            if(distance < closestDistance){
                closestDistance = distance;
                closestIndex = i;
                closestVec = pos;
            }
        }

        if(closestIndex == -1){
            return null;
        }

        return new ComputeContext(
                closestIndex,
                cs.in(closestIndex),
                BlockPos.containing(blockCenter) ,
                closestVec,
                closestDistance,
                true
        );
    }

    public static ComputeContext closestOutput(ConnectionStatus cs, Vec3 viewHitVec, Direction horizontal, Direction vertical, Vec3 blockCenter){
        int closestIndex = -1;
        double closestDistance = Double.MAX_VALUE;
        Vec3 closestVec = null;

        for(int i = 0; i < cs.outputs.size(); i++){
            Vec3 offset = computeOutputPortOffset(horizontal, vertical, i, cs.outputs.size());
            Vec3 pos = blockCenter.add(offset);
            double distance = pos.distanceToSqr(viewHitVec);
            if(distance < closestDistance){
                closestDistance = distance;
                closestIndex = i;
                closestVec = pos;
            }
        }
        if(closestIndex == -1){
            return null;
        }
        return new ComputeContext(
                    closestIndex,
                    cs.out(closestIndex),
                    BlockPos.containing(blockCenter),
                    closestVec,
                    closestDistance,
                    false
        )
                ;
    }

    private static @Nullable ClientViewContext compareAndMakeContext(
            @Nullable ComputeContext closestInput,
            @Nullable ComputeContext closestOutput
    ){
        ComputeContext winner = null;
        if(closestInput == null && closestOutput == null)return null;
        if(closestInput == null)winner = closestOutput;
        else if(closestOutput == null)winner = closestInput;
        else if(closestInput.result < closestOutput.result)winner = closestInput;
        else winner = closestOutput;

        return new ClientViewContext(
                    winner.pos,
                    winner.portName,
                    winner.isInput,
                    winner.portPos
            );
    }

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
        ConnectionStatus cs = cbe.readClientConnectionStatus();
        if(cs == null)return null;
        ComputeContext closestInput = closestInput(cs, viewHitVec, cbe.getHorizontal(), cbe.getVertical(), cbe.getBlockPos().getCenter());
        ComputeContext closestOutput = closestOutput(cs, viewHitVec, cbe.getHorizontal(), cbe.getVertical(), cbe.getBlockPos().getCenter());

        return compareAndMakeContext(closestInput, closestOutput);
    }


    public record ComputeContext(int id, String portName, BlockPos pos, Vec3 portPos, double result, boolean isInput){}
}
