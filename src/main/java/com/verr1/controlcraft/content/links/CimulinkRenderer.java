package com.verr1.controlcraft.content.links;

import com.mojang.datafixers.util.Pair;
import com.simibubi.create.CreateClient;
import com.simibubi.create.foundation.blockEntity.behaviour.ValueBox;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftClient;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.links.*;
import com.verr1.controlcraft.foundation.data.render.CimulinkWireEntry;
import com.verr1.controlcraft.foundation.managers.render.CimulinkRenderCenter;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.ships.Ship;

import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.IntStream;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;
import static com.verr1.controlcraft.utils.MinecraftUtils.toVec3;

public class CimulinkRenderer implements IRenderer{

    private final CimulinkBlockEntity<?> cbe;

    private final Map<String, RenderCurveKey> cachedKeys = new HashMap<>();
    ConnectionStatus cachedConnectionStatus = ConnectionStatus.EMPTY;

    ValueStatus cachedCurrentValueStatus = ValueStatus.EMPTY;
    ValueStatus cachedPreviousValueStatus = ValueStatus.EMPTY;



    final List<Vec3> socketPositions = Collections.synchronizedList(new ArrayList<>());

    public CimulinkRenderer(CimulinkBlockEntity<?> cbe) {
        this.cbe = cbe;
    }

    void tickCached(){
        cachedConnectionStatus = Optional.ofNullable(cbe.readClientConnectionStatus()).orElse(ConnectionStatus.EMPTY);
        cachedPreviousValueStatus = cachedCurrentValueStatus;
        cachedCurrentValueStatus = Optional.ofNullable(cbe.readClientValueStatus()).orElse(ValueStatus.EMPTY);
    }

    List<Integer> changedInput(){
        if(cachedCurrentValueStatus == null || cachedPreviousValueStatus == null)return List.of();
        if(cachedCurrentValueStatus.inputValues.size() != cachedPreviousValueStatus.inputValues.size())return List.of();
        int size = cachedCurrentValueStatus.inputValues.size();
        return IntStream.range(0, size)
                .filter(i -> !Objects.equals(
                        cachedCurrentValueStatus.inputValues.get(i),
                        cachedPreviousValueStatus.inputValues.get(i))
                )
                .boxed()
                .toList();
    }

    public List<Vec3> socketPositions() {
        return socketPositions;
    }

    private void flash(List<Integer> changedInput){
        if(cbe.getLevel() ==null || !cbe.getLevel().isClientSide)return;
        changedInput.stream().filter(i -> i < n())
                .map(cs().inputs::get)
                .map(cachedKeys::get)
                .map(ControlCraftClient.CLIENT_CURVE_OUTLINER::retrieveLine)
                .filter(CimulinkWireEntry.class::isInstance)
                .map(CimulinkWireEntry.class::cast)
                .forEach(CimulinkWireEntry::flash);
    }

    public Vec3 socketRenderOffset(){
        return toVec3(cbe.getDirection().getNormal()).scale(-0.4);
    }

    void tickSocketPositions(){
        synchronized (socketPositions) {
            socketPositions.clear();
            cs().inputPorts.keySet().forEach(inName -> {
                Vec3 offset = computeInputPortOffset(in(inName));
                socketPositions.add(offset.add(socketRenderOffset()));
            });
            cs().outputPorts.keySet().forEach(outName -> {
                Vec3 offset = computeOutputPortOffset(out(outName));
                socketPositions.add(offset.add(socketRenderOffset()));
            });
        }
    }

    public void tick(){
        tickCached();
        tickBox();
        tickCurve();
        tickFlash();
    }

    @NotNull
    ConnectionStatus cs(){
        return cachedConnectionStatus;
    }

    @NotNull ValueStatus vs(){
        return cachedCurrentValueStatus;
    }

    int in(String name){
        return cs().inputs.indexOf(name);
    }

    int out(String name){
        return cs().outputs.indexOf(name);
    }

    int m(){
        return cs().outputs.size();
    }

    int n(){
        return cs().inputs.size();
    }


    public Pair<Float, Float> computeLocalOffset(ClientViewContext cvc){
        if(cvc.isInput()){
            int index = cs().inputs.indexOf(cvc.portName());
            if(index == -1){
                ControlCraft.LOGGER.error("Failed to find input port index for rendering");
                return Pair.of(0.0f, 0.0f);
            }
            return Pair.of(
                    -(float)deltaX(index, cs().inputs.size()),
                    (float)deltaY(index, cs().inputs.size())
            );
        }else{
            int index = cs().outputs.indexOf(cvc.portName());
            if(index == -1){
                ControlCraft.LOGGER.error("Failed to find output port index for rendering");
                return Pair.of(0.0f, 0.0f);
            }
            return Pair.of(
                    (float)deltaX(index, cs().outputs.size()),
                    (float)deltaY(index, cs().outputs.size())
            );
        }

    }
    private static final int MAX_INPUTS_PER_COLUMN = 4;

    private static double deltaX(int count, int total){
        int div = count / MAX_INPUTS_PER_COLUMN; // which column
        int m_1 = (total / MAX_INPUTS_PER_COLUMN + 1); // column count
        double dx = 0.5 / m_1;
        return 0.35 - div * dx;
    }

    private static double deltaY(int count, int total){
            /*
            double dy = 1.0 / total;
            return (total - 1) * dy / 2 - (count * dy);
            * */
        int total_mod = total % MAX_INPUTS_PER_COLUMN;

        int m_1 = count < total - total_mod ? MAX_INPUTS_PER_COLUMN : total_mod; // row count

        int mod = (count % MAX_INPUTS_PER_COLUMN);

        double dy = 1.0 / m_1;

        return (m_1 - 1) * dy / 2 - (mod * dy);
    }

    public Vec3 computeOutputPortOffset(int count){
        double x = deltaX(count, m());//0.25;
        double y = deltaY(count, m());
        Vec3 h = MinecraftUtils.toVec3(cbe.getHorizontal().getNormal());
        Vec3 v = MinecraftUtils.toVec3(cbe.getVertical().getNormal());
        return h.scale(x).add(v.scale(y));
    }

    public Vec3 computeInputPortOffset(int count){
        double x = -deltaX(count, m());//-0.25;
        double y =  deltaY(count, n());

        Vec3 h = MinecraftUtils.toVec3(cbe.getHorizontal().getNormal());
        Vec3 v = MinecraftUtils.toVec3(cbe.getVertical().getNormal());
        return h.scale(x).add(v.scale(y));
    }

    public @Nullable CimulinkRenderCenter.ComputeContext closestInput(Vec3 viewHitVec){
        int closestIndex = -1;
        double closestDistance = Double.MAX_VALUE;
        Vec3 closestVec = null;

        for(int i = 0; i < n(); i++){
            Vec3 offset = computeInputPortOffset(i);
            Vec3 pos = center().add(offset);
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

        return new CimulinkRenderCenter.ComputeContext(
                closestIndex,
                cs().in(closestIndex),
                cbe.getBlockPos(),
                closestVec,
                closestDistance,
                true
        );
    }

    public CimulinkRenderCenter.ComputeContext closestOutput(Vec3 viewHitVec){
        int closestIndex = -1;
        double closestDistance = Double.MAX_VALUE;
        Vec3 closestVec = null;

        for(int i = 0; i < m(); i++){
            Vec3 offset = computeOutputPortOffset(i);
            Vec3 pos = center().add(offset);
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
        return new CimulinkRenderCenter.ComputeContext(
                closestIndex,
                cs().out(closestIndex),
                cbe.getBlockPos(),
                closestVec,
                closestDistance,
                false
        )
                ;
    }

    private static @Nullable ClientViewContext compareAndMakeContext(
            @Nullable CimulinkRenderCenter.ComputeContext closestInput,
            @Nullable CimulinkRenderCenter.ComputeContext closestOutput
    ){
        CimulinkRenderCenter.ComputeContext winner = null;
        if(closestInput == null && closestOutput == null)return null;
        if(closestInput == null)winner = closestOutput;
        else if(closestOutput == null)winner = closestInput;
        else if(closestInput.result() < closestOutput.result())winner = closestInput;
        else winner = closestOutput;

        return new ClientViewContext(
                winner.pos(),
                winner.portName(),
                winner.isInput(),
                winner.portPos()
        );
    }

    public Vec3 transformIfIncludeShip(Vec3 wc){
        Ship s = cbe.getShipOn();
        return Optional.ofNullable(s)
                .map(Ship::getWorldToShip)
                .map(t -> t.transformPosition(toJOML(wc)))
                .map(ValkyrienSkies::toMinecraft)
                .orElse(wc);
    }


    // given a cbe to check and a viewHitVec, return the closest looking port pos and name index
    public @Nullable ClientViewContext computeContext(@NotNull Vec3 viewHitVec, boolean transform){
        viewHitVec = transform ? transformIfIncludeShip(viewHitVec): viewHitVec;
        CimulinkRenderCenter.ComputeContext closestInput = closestInput(viewHitVec);
        CimulinkRenderCenter.ComputeContext closestOutput = closestOutput(viewHitVec);

        return compareAndMakeContext(closestInput, closestOutput);
    }

    public @Nullable Vec3 outPosition(String name){
        int index = out(name);
        if(index == -1)return null;
        Vec3 offset = computeOutputPortOffset(index);
        return center().add(offset);
    }

    public @Nullable Vec3 inPosition(String name){
        int index = in(name);
        if(index == -1)return null;
        Vec3 offset = computeInputPortOffset(index);
        return center().add(offset);
    }

    public Vec3 center(){
        return cbe.getBlockPos().getCenter();
    }

    public Direction facing(){
        return cbe.getDirection();
    }


    public void tickCurve(){
        cs().inputPorts.forEach((inName, value) -> {
            Vec3 inPosition = inPosition(inName);
            Direction inDir = facing();
            if(inPosition == null)return;
            BlockPos outPos = value.pos().pos();
            CimulinkRenderer ord = of(outPos);
            if(ord == null)return;
            String outName = value.portName();
            Direction outDir = ord.facing();
            Vec3 outPosition = ord.outPosition(outName);
            if(outPosition == null)return;

            BlockPort inPort = new BlockPort(WorldBlockPos.of(cbe.getLevel(), cbe.getBlockPos()), inName);
            BlockPort outPort = new BlockPort(WorldBlockPos.of(cbe.getLevel(), outPos), outName);

            var k = new RenderCurveKey(inPort, outPort, inPosition, outPosition, inDir, outDir);

            ControlCraftClient.CLIENT_CURVE_OUTLINER.showLine(k, k::createBezier);

            cachedKeys.put(inName, k);
        });
    }

    public void tickFlash(){
        flash(changedInput());
    }

    public static CimulinkRenderer of(BlockPos clientPos){
        return BlockEntityGetter.getLevelBlockEntityAt(
                        Minecraft.getInstance().level,
                        clientPos,
                        CimulinkBlockEntity.class
                )
                .map(CimulinkBlockEntity::renderer)
                .filter(CimulinkRenderer.class::isInstance)
                .map(CimulinkRenderer.class::cast)
                .orElse(null);
    }

    public void tickBox() {
        if(!cbe.beingLookedAt())return;

        HitResult target = Minecraft.getInstance().hitResult;

        if (!(target instanceof BlockHitResult result) || cbe.getLevel() == null)
            return;

        if(result.getDirection() != facing())return;

        ClientViewContext cvc = computeContext(target.getLocation(), true);
        if(cvc == null)return;

        String portName = cvc.portName();
        ValueStatus vs = cbe.readClientValueStatus();

        double val = -1;
        if(vs != null){
            try{
                val = cvc.isInput() ?
                        vs.inputValues.get(in(cvc.portName()))
                        :
                        vs.outputValues.get(out(cvc.portName()));
            }catch (IndexOutOfBoundsException ignored) {}
        }

        AABB bb = new AABB(Vec3.ZERO, Vec3.ZERO).inflate(.15f);
        MutableComponent label = Component.literal(portName + " ");
        MutableComponent inout = cvc.isInput() ? Component.literal("Input: ") : Component.literal("Output: ");
        MutableComponent value = Component.literal("[" + "%.4f".formatted(val) + "]").withStyle(s -> s.withUnderlined(true).withColor(ChatFormatting.DARK_AQUA));

        ValueBox box = new ValueBox(label, bb, cbe.getBlockPos());
        var xy = computeLocalOffset(cvc);
        LinkPortSlot transform =
                (LinkPortSlot)new LinkPortSlot(
                        xy.getFirst() * 16,
                        xy.getSecond() * 16
                ).fromSide(cbe.getDirection()); //

        CreateClient.OUTLINER
                .showValueBox(cbe.getBlockPos(), box.transform(transform))
                .highlightFace(result.getDirection());



        List<MutableComponent> tip = new ArrayList<>();
        tip.add(inout.append(label).append(value));
        CreateClient.VALUE_SETTINGS_HANDLER.showHoverTip(tip);
    }

}


