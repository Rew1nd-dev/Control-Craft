package com.verr1.controlcraft.foundation.managers;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftClient;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.links.BlockPort;
import com.verr1.controlcraft.foundation.data.links.ClientViewContext;
import com.verr1.controlcraft.foundation.data.links.ConnectionStatus;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nullable;
import java.awt.*;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@OnlyIn(Dist.CLIENT)
public class CimulinkRenderCenter {
    static final double DELTA_Y = 0.2;

    public static Vec3 computeOutputPortOffset(Direction horizontal, Direction vertical, int count, int total){
        double x = 0.25;
        double y = (total - 1) * DELTA_Y / 2 - (count * DELTA_Y / 2);
        Vec3 h = MinecraftUtils.toVec3(horizontal.getNormal());
        Vec3 v = MinecraftUtils.toVec3(vertical.getNormal());
        return h.scale(x).add(v.scale(y));
    }

    public static Vec3 computeInputPortOffset(Direction horizontal, Direction vertical, int count, int total){
        double x = -0.25;
        double y = (total - 1) * DELTA_Y / 2 - (count * DELTA_Y);

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
        ComputeContext closestInput = closestInput(cs, viewHitVec, cbe.getHorizontal(), cbe.getVertical(), cbe.getFaceCenter());
        ComputeContext closestOutput = closestOutput(cs, viewHitVec, cbe.getHorizontal(), cbe.getVertical(), cbe.getFaceCenter());

        return compareAndMakeContext(closestInput, closestOutput);
    }


    public record ComputeContext(int id, String portName, BlockPos pos, Vec3 portPos, double result, boolean isInput){}

    public static void renderOutConnection(BlockPos pos, String portName){
        CimulinkBlockEntity<?> cbe = of(pos);
        if(cbe == null)return;
        ConnectionStatus cs = cbe.readClientConnectionStatus();
        Set<BlockPort> ins = cs.outputPorts.get(portName);
        if(ins == null)return;
        BlockPort out = new BlockPort(WorldBlockPos.of(Minecraft.getInstance().level, pos), portName);
        ins.forEach(in -> {
            Optional.ofNullable(RenderedConnection.of(out, in)).ifPresent(r -> r.render(pos.toShortString() + portName + in.portName(), 40, Color.GREEN));
        });
    }

    public static void renderInConnection(BlockPos pos, String portName){
        CimulinkBlockEntity<?> cbe = of(pos);
        if(cbe == null)return;
        ConnectionStatus cs = cbe.readClientConnectionStatus();
        BlockPort out = cs.inputPorts.get(portName);
        if(out == null)return;
        BlockPort in = new BlockPort(WorldBlockPos.of(Minecraft.getInstance().level, pos), portName);
        Optional.ofNullable(RenderedConnection.of(out, in)).ifPresent(r -> r.render(pos.toShortString() + portName, 40, Color.GREEN));
    }

    public static @Nullable CimulinkBlockEntity<?> of(BlockPos clientPos){

        return (CimulinkBlockEntity<?>) BlockEntityGetter.getLevelBlockEntityAt(
                        Minecraft.getInstance().level,
                        clientPos,
                        CimulinkBlockEntity.class
                )
                .orElse(null);
    }

    public static class RenderedConnection{
        public List<Vec3> points;

        public RenderedConnection(List<Vec3> points) {
            this.points = points;
        }


        public static RenderedConnection of(BlockPort out, BlockPort in){
            CimulinkBlockEntity<?> cbo = CimulinkRenderCenter.of(out.pos().pos());
            CimulinkBlockEntity<?> cbi = CimulinkRenderCenter.of(in.pos().pos());
            if(cbo == null || cbi == null) return null;
            ConnectionStatus cso = cbo.readClientConnectionStatus();
            ConnectionStatus csi = cbi.readClientConnectionStatus();
            if(csi == null || cso == null) return null;
            int indexO = cso.outputs.indexOf(out.portName());
            Vec3 outPos = cbo.getFaceCenter().add(
                    computeOutputPortOffset(
                            cbo.getHorizontal(),
                            cbo.getVertical(),
                            indexO,
                            cso.outputs.size()
                    )
            );
            int indexI = csi.inputs.indexOf(in.portName());
            Vec3 inPos = cbi.getFaceCenter().add(
                    computeInputPortOffset(
                            cbi.getHorizontal(),
                            cbi.getVertical(),
                            indexI,
                            csi.inputs.size()
                    )
            );
            if(indexI == -1 || indexO == -1){
                ControlCraft.LOGGER.error("Failed to find input or output port index for connection rendering");
            }
            return of(outPos, inPos,
                    cbo.getHorizontal(), cbo.getVertical(),
                    cbi.getHorizontal(), cbi.getVertical()
            );
        }

        public void render(String slot, int ticks, Color color){
            ControlCraftClient.CLIENT_LERPED_OUTLINER.showLine(slot,
                    points.get(0),
                    points.get(1),
                    ticks
            ).colored(color.getRGB());
        }

        public static RenderedConnection of(Vec3 out, Vec3 in,
                                            Direction hOut, Direction vOut,
                                            Direction hIn, Direction vIn
        ){
            return new RenderedConnection(List.of(out, in)); // TODO: implement proper connection rendering
        }

    }

}
