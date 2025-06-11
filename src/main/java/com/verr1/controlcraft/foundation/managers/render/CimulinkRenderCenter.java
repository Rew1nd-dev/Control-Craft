package com.verr1.controlcraft.foundation.managers.render;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
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

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;

@OnlyIn(Dist.CLIENT)
public class CimulinkRenderCenter {






    /*
    *
    public static void tick() {
        // tickCurves();
        Minecraft mc = Minecraft.getInstance();
        HitResult target = mc.hitResult;
        ClientLevel world = mc.level;
        if (!(target instanceof BlockHitResult result) || world == null)
            return;


        BlockPos pos = result.getBlockPos();

        CimulinkBlockEntity<?> cbe = of(pos);
        if (cbe == null)return;
        if (cbe.getDirection() != result.getDirection())return; // not looking at face
        ConnectionStatus cs = cbe.readClientConnectionStatus();
        if (cs == null)return;
        ClientViewContext cvc = computeContext(pos, target.getLocation(), world);
        if(cvc == null)return;

        String portName = cvc.portName();

        ValueStatus vs = cbe.readClientValueStatus();
        double val = -1;
        if(vs != null){
            try{
                val = cvc.isInput() ?
                        vs.inputValues.get(cs.inputs.indexOf(cvc.portName()))
                        :
                        vs.outputValues.get(cs.outputs.indexOf(cvc.portName()));
            }catch (IndexOutOfBoundsException ignored) {}
        }

        AABB bb = new AABB(Vec3.ZERO, Vec3.ZERO).inflate(.25f);
        MutableComponent label = Component.literal(portName + " ");
        MutableComponent inout = cvc.isInput() ? Component.literal("Input: ") : Component.literal("Output: ");
        MutableComponent value = Component.literal("[" + "%.4f".formatted(val) + "]").withStyle(s -> s.withUnderlined(true).withColor(ChatFormatting.DARK_AQUA));

        ValueBox box = new ValueBox(label, bb, pos).wideOutline();
        var xy = computeLocalOffset(cvc, cs);
        LinkPortSlot transform =
                (LinkPortSlot)new LinkPortSlot(
                    xy.getFirst() * 16,
                    xy.getSecond() * 16
                ).fromSide(cbe.getDirection()); //

        CreateClient.OUTLINER
                .showValueBox(pos, box.transform(transform))
                .highlightFace(result.getDirection());



        List<MutableComponent> tip = new ArrayList<>();
        tip.add(inout.append(label).append(value));
        CreateClient.VALUE_SETTINGS_HANDLER.showHoverTip(tip);
    }
    * public static Pair<Float, Float> computeLocalOffset(ClientViewContext cvc, ConnectionStatus cs){
        if(cvc.isInput()){
            int index = cs.inputs.indexOf(cvc.portName());
            if(index == -1){
                ControlCraft.LOGGER.error("Failed to find input port index for rendering");
                return Pair.of(0.0f, 0.0f);
            }
            return Pair.of(-0.25f, (float)deltaY(index, cs.inputs.size()));
        }else{
            int index = cs.outputs.indexOf(cvc.portName());
            if(index == -1){
                ControlCraft.LOGGER.error("Failed to find output port index for rendering");
                return Pair.of(0.0f, 0.0f);
            }
            return Pair.of(0.25f, (float)deltaY(index, cs.outputs.size()));
        }

    }

    private static double deltaY(int count, int total){
        double dy = 1.0 / total;
        return (total - 1) * dy / 2 - (count * dy);
    }

    public static Vec3 computeOutputPortOffset(Direction horizontal, Direction vertical, int count, int total){
        double x = 0.15;
        double y = deltaY(count, total);
        Vec3 h = MinecraftUtils.toVec3(horizontal.getNormal());
        Vec3 v = MinecraftUtils.toVec3(vertical.getNormal());
        return h.scale(x).add(v.scale(y));
    }

    public static Vec3 computeInputPortOffset(Direction horizontal, Direction vertical, int count, int total){
        double x = -0.25;
        double y = deltaY(count, total);

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
    *
    * */

    // given a cbe to check and a viewHitVec, return the closest looking port pos and name index
    public static @Nullable ClientViewContext computeContext(
            @NotNull BlockPos cbePos,
            @NotNull Vec3 viewHitVec,
            @NotNull Level world
    ){
        /*
        CimulinkBlockEntity<?> cbe =
                BlockEntityGetter.getLevelBlockEntityAt(world, cbePos, CimulinkBlockEntity.class)
                .orElse(null);
        if(cbe == null)return null;
        ConnectionStatus cs = cbe.readClientConnectionStatus();
        if(cs == null)return null;
        ComputeContext closestInput = closestInput(cs, viewHitVec, cbe.getHorizontal(), cbe.getVertical(), cbe.getFaceCenter());
        ComputeContext closestOutput = closestOutput(cs, viewHitVec, cbe.getHorizontal(), cbe.getVertical(), cbe.getFaceCenter());

        return compareAndMakeContext(closestInput, closestOutput);
        * */
        CimulinkBlockEntity<?> cbe =
                BlockEntityGetter.getLevelBlockEntityAt(world, cbePos, CimulinkBlockEntity.class)
                .orElse(null);
        if(cbe == null)return null;
        return cbe.renderer().computeContext(viewHitVec);
    }


    public record ComputeContext(int id, String portName, BlockPos pos, Vec3 portPos, double result, boolean isInput){}


/*
*
* public static class LinkPortSlot extends ValueBoxTransform.Sided {
        // transform is set in ValueBox, and state is Blockstate at ValueBox::pos

        private float y = 8;
        private float x = 3;

        public LinkPortSlot(float x, float y){
            this.y = y;
            this.x = x;
        }


        @Override
        protected Vec3 getSouthLocation() {
            int signY = direction == Direction.DOWN ? -1 : 1;
            int signX = direction.getAxisDirection() == Direction.AxisDirection.POSITIVE ? 1 : -1;
            return VecHelper.voxelSpace(8 + signX * x, 8 + signY * y, 8);
        }

    }
*
* public static final ConcurrentHashMap<RenderCurveKey, Integer> CURVE2LIVES = new ConcurrentHashMap<>();

    public static void keep(RenderCurveKey k){
        ControlCraftClient.CLIENT_CURVE_OUTLINER.showLine(k, k::createBezier);
        // CURVE2LIVES.put(k, 20);
    }

    public static void tickCurves(){
        CURVE2LIVES.entrySet().forEach(e -> e.setValue(e.getValue() - 1));
        CURVE2LIVES.entrySet()
                .stream()
                .filter(e -> e.getValue() < 0)
                .toList()
                .forEach(e -> CURVE2LIVES.remove(e.getKey()));
        tickRender();
    }

    public static void tickRender(){
        CURVE2LIVES.keySet().forEach( k ->
            ControlCraftClient.CLIENT_CURVE_OUTLINER.showLine(k, k::createBezier)
        );
    }
    * public static class RenderedConnection{
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
    * public record RenderCurveKey(
            BlockPos inPos, int inIndex, int inCount, Direction inDir, Direction inHorizontal, Direction inVertical,
            BlockPos outPos, int outIndex, int outCount, Direction outDir, Direction outHorizontal, Direction outVertical
    ){
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof RenderCurveKey that)) return false;
            if (inIndex != that.inIndex) return false;
            if (outIndex != that.outIndex) return false;
            if (!inDir.equals(that.inDir)) return false;
            if (!outDir.equals(that.outDir)) return false;
            if (!inPos.equals(that.inPos)) return false;
            if (!outPos.equals(that.outPos)) return false;
            if (inCount != that.inCount) return false;
            if (!inHorizontal.equals(that.inHorizontal)) return false;
            if (!inVertical.equals(that.inVertical)) return false;
            if (!outHorizontal.equals(that.outHorizontal)) return false;
            if (!outVertical.equals(that.outVertical)) return false;

            return outCount == that.outCount;
        }

        public RenderableOutline createBezier(){
            Vector3dc inJoml = toJOML(Vec3.atLowerCornerOf(inDir.getNormal()));
            Vector3dc outJoml = toJOML(Vec3.atLowerCornerOf(outDir.getNormal()));
            return new FancyBezierCurveEntry(
                    toJOML(outPos.getCenter().add(computeOutputPortOffset(outHorizontal, outVertical, outIndex, outCount))).fma(-0.5, outJoml),
                    toJOML(inPos.getCenter().add(computeInputPortOffset(inHorizontal, inVertical, inIndex, inCount))).fma(-0.5, inJoml),
                    outJoml,
                    inJoml,
                    0.067f,
                    20
            );
        }

        @Override
        public int hashCode() {
            return Objects.hashCode(inPos, inIndex, inCount, outPos, outIndex, outCount, inDir, outDir);
        }
    }

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
* */




    public static @Nullable CimulinkBlockEntity<?> of(BlockPos clientPos){

        return (CimulinkBlockEntity<?>) BlockEntityGetter.getLevelBlockEntityAt(
                        Minecraft.getInstance().level,
                        clientPos,
                        CimulinkBlockEntity.class
                )
                .orElse(null);
    }



}
