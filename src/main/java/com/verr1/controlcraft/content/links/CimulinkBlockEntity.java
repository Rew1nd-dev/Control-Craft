package com.verr1.controlcraft.content.links;

import com.simibubi.create.content.equipment.goggles.IHaveGoggleInformation;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.NetworkBlockEntity;
import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ILinkableBlock;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.links.ClientViewContext;
import com.verr1.controlcraft.foundation.data.links.ConnectionStatus;
import com.verr1.controlcraft.foundation.data.links.ValueStatus;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.MinecraftUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import kotlin.Pair;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;

public abstract class CimulinkBlockEntity<T extends BlockLinkPort> extends NetworkBlockEntity implements
        ILinkableBlock, IHaveGoggleInformation
{

    private final T linkPort;

    private final Map<Integer, Vec3> cachedInputViewOffsets = new HashMap<>();
    private final Map<Integer, Vec3> cachedOutputViewOffsets = new HashMap<>();

    @Override
    public final void initialize() {
        super.initialize();

        syncForAllPlayers(false, SharedKeys.CONNECTION_STATUS, SharedKeys.VALUE_STATUS);
        initializeExtra();
    }


    protected void initializeExtra(){

    }

    protected abstract T create(@NotNull Level level, BlockPos pos);

    public CimulinkBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        linkPort = create(ControlCraftServer.OVERWORLD, getBlockPos());
        buildRegistry(SharedKeys.BLP)
                .withBasic(CompoundTagPort.of(
                        () -> linkPort().serialize(),
                        t -> linkPort().deserialize(t)
                ))
                .register();

        buildRegistry(SharedKeys.COMPONENT_NAME)
                .withBasic(SerializePort.of(
                        this::name,
                        this::setName,
                        SerializeUtils.STRING
                ))
                .withClient(ClientBuffer.STRING.get())
                .dispatchToSync()
                .register();

        registerPartial(
                SharedKeys.CONNECTION_STATUS,
                () -> ConnectionStatus.summarize(linkPort()),
                ConnectionStatus::deserialize,
                ConnectionStatus.class
        );

        registerPartial(
                SharedKeys.VALUE_STATUS,
                () -> ValueStatus.summarize(linkPort()),
                ValueStatus::deserialize,
                ValueStatus.class
        );
    }

    public void setName(String name){
        if(linkPort() == null)return;
        linkPort().setName(name);
    }

    public String name(){
        if(linkPort() == null)return "why?";
        return linkPort().name();
    }

    private<R> void registerPartial(
            NetworkKey key,
            Supplier<CompoundTag> ser,
            Function<CompoundTag, R> d_ser,
            Class<R> clazz
    ){
        buildRegistry(key)
                .withBasic(CompoundTagPort.of(
                        ser,
                        $ -> {}
                ))
                .withClient(new ClientBuffer<>(
                        SerializeUtils.of(
                                $ -> new CompoundTag(),
                                d_ser
                        ),
                        clazz
                ))
                .dispatchToSync()
                .runtimeOnly()
                .register();
    }

    @OnlyIn(Dist.CLIENT)
    private void requestConnectionStatusOnFocus(){
        BlockPos p = MinecraftUtils.lookingAtPos();
        if(p == null || !p.equals(getBlockPos()))return;
        handler().request(SharedKeys.CONNECTION_STATUS);
    }

    @OnlyIn(Dist.CLIENT)
    private boolean beingLookingAt(){
        BlockPos p = MinecraftUtils.lookingAtPos();
        return p != null && p.equals(getBlockPos());
    }

    @OnlyIn(Dist.CLIENT)
    private void requestValueStatusOnFocus(){
        if(!beingLookingAt())return;
        handler().request(SharedKeys.VALUE_STATUS);
    }

    @Override
    public void tickClient() {
        super.tickClient();
        requestValueStatusOnFocus();
    }

    @Override
    public void remove() {
        super.remove();
        linkPort().quit();
    }

    @Override
    public void lazyTickClient() {
        super.lazyTickClient();
        requestConnectionStatusOnFocus();
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        if(linkPort() == null)return;
        //TODO: This may cause save loads can't restore links, so change it later
        linkPort().removeInvalid();
        syncForNear(false, SharedKeys.COMPONENT_NAME);
    }

    public ConnectionStatus readClientConnectionStatus(){
        return handler().readClientBuffer(SharedKeys.CONNECTION_STATUS, ConnectionStatus.class);
    }

    public ValueStatus readClientValueStatus(){
        return handler().readClientBuffer(SharedKeys.VALUE_STATUS, ValueStatus.class);
    }

    public String readClientComponentName(){
        return handler().readClientBuffer(SharedKeys.COMPONENT_NAME, String.class);
    }

    public @NotNull Direction getDirection(){
        if(getBlockState().hasProperty(BlockStateProperties.FACING)) return getBlockState().getValue(BlockStateProperties.FACING);
        return Direction.UP;
    }

    public Direction getVertical(){
        Direction dir = getDirection();
        return MinecraftUtils.getVerticalDirectionSimple(dir);
    }

    public Direction getHorizontal(){
        return getVertical().getClockWise(getDirection().getAxis());
    }



    private static Vec3 computePortOffset(Direction horizontal, Direction Vertical, int count, boolean isInput){
        horizontal = isInput ? horizontal.getOpposite(): horizontal;
        double x = 0.25;
        double dy = 0.15;
        int total = 4;
        double yLen = (total - 1) * dy;
        double y = yLen / 2 - count * dy;
        return  MinecraftUtils.toVec3(horizontal.getNormal()).scale(x).add(
                MinecraftUtils.toVec3(Vertical.getNormal()).scale(y)
        );
    }

    @OnlyIn(Dist.CLIENT)
    private Vec3 getInputPortOffset(int index){
        return cachedInputViewOffsets.computeIfAbsent(index, i -> computePortOffset(getHorizontal(), getVertical(), i, true));
    }

    @OnlyIn(Dist.CLIENT)
    private Vec3 getOutputPortOffset(int index){
        return cachedOutputViewOffsets.computeIfAbsent(index, i -> computePortOffset(getHorizontal(), getVertical(), i, false));
    }
    @OnlyIn(Dist.CLIENT)
    private int closestInput(Vec3 lookingAtVec, int totalInputCount){
        return IntStream
                .range(0, totalInputCount)
                .boxed()
                .min(Comparator.comparingDouble(
                        i -> lookingAtVec.distanceToSqr(getInputPortOffset(i))
                    )
                )
                .orElse(0);
    }

    @OnlyIn(Dist.CLIENT)
    private int closestOutput(Vec3 lookingAtVec, int totalOutputCount){
        return IntStream
                .range(0, totalOutputCount)
                .boxed()
                .min(Comparator.comparingDouble(
                                i -> lookingAtVec.distanceToSqr(getOutputPortOffset(i))
                        )
                ).orElse(0);
    }


    @OnlyIn(Dist.CLIENT)
    public @Nullable ClientViewContext clientViewMap(){
        Vec3 lookingAtVec = MinecraftUtils.lookingAtVec();
        BlockPos lookingAtPos = MinecraftUtils.lookingAtPos();
        if (lookingAtVec == null || lookingAtPos == null || !lookingAtPos.equals(getBlockPos()))return null;
        return clientViewMap(this, lookingAtVec);
    }

    public static ClientViewContext clientViewMap(CimulinkBlockEntity<?> be, Vec3 lookingAtVec){
        ConnectionStatus cs = be.readClientConnectionStatus();
        if(cs == null)return null;
        Vec3 viewOffset = lookingAtVec.subtract(be.getBlockPos().getCenter());
        int inputIndex = be.closestInput(viewOffset, cs.inputs.size());
        int outputIndex = be.closestOutput(viewOffset, cs.outputs.size());
        double inputOffset = be.getInputPortOffset(inputIndex).subtract(viewOffset).lengthSqr();
        double outputOffset = be.getOutputPortOffset(outputIndex).subtract(viewOffset).lengthSqr();
        return inputOffset < outputOffset ?
                new ClientViewContext(be.getBlockPos(), cs.in(inputIndex), true, be.getInputPortOffset(inputIndex))
                :
                new ClientViewContext(be.getBlockPos(), cs.out(outputIndex), false, be.getOutputPortOffset(outputIndex));
    }

    @Override
    public T linkPort(){
        return linkPort;
    }

    @Override
    public boolean addToGoggleTooltip(List<Component> tooltip, boolean isPlayerSneaking) {
        if(level == null)return false;
        if(isPlayerSneaking){
            tooltip.addAll(makeDetailedToolTip(readClientConnectionStatus(), level));
        }else{
            tooltip.addAll(makeToolTip(readClientValueStatus(), readClientConnectionStatus()));
        }
        return true;
    }

    public static List<Component> makeToolTip(ValueStatus vs, ConnectionStatus cs){
        if(vs == null || cs == null)return List.of();
        int inSize = Math.min(vs.inputValues.size(), cs.inputs.size());
        int outSize = Math.min(vs.outputValues.size(), cs.outputs.size());
        Component inTitle = Component.literal("Inputs").withStyle(s -> s.withColor(ChatFormatting.DARK_BLUE));
        Component outTitle = Component.literal("Outputs").withStyle(s -> s.withColor(ChatFormatting.DARK_RED));

        StringBuilder sbi = new StringBuilder();
        IntStream.range(0, inSize).forEach(
                i -> sbi.append(cs.inputs.get(i)).append(": ").append(vs.inputValues.get(i)).append(" | ")
        );
        StringBuilder sbo = new StringBuilder();
        IntStream.range(0, outSize).forEach(
                i -> sbo.append(cs.outputs.get(i)).append(": ").append(vs.outputValues.get(i)).append(" | ")
        );

        return List.of(
                Component.literal("    Values:").withStyle(ChatFormatting.GOLD),
                inTitle,
                Component.literal(sbi.toString()).withStyle(ChatFormatting.GREEN),
                outTitle,
                Component.literal(sbo.toString()).withStyle(ChatFormatting.DARK_GREEN)
        );
    }

    public static List<Component> makeDetailedToolTip(ConnectionStatus cs, Level world){
        if(cs == null)return List.of();
        MutableComponent in = Component.literal("in: \n");
        cs.inputPorts.forEach((inName, outBp) -> {
            in.append(inName).append(" <- ")
                    .append(ConnectionStatus.mapToName(outBp.pos().pos(), world)).append("-")
                    .append(outBp.portName()).append("\n");
        });
        MutableComponent out = Component.literal("out: \n");
        cs.outputPorts.forEach((outName, inBps) -> {
            out.append(outName).append("->");
            MutableComponent bps = Component.literal("");
            inBps.forEach(bp -> bps.append(ConnectionStatus.mapToName(bp.pos().pos(), world)).append("-").append(bp.portName()).append(", "));
            out.append(bps).append("\n");
        });
        return List.of(
                Component.literal("    Connection Status:").withStyle(ChatFormatting.GOLD),
                in.withStyle(ChatFormatting.GREEN),
                out.withStyle(ChatFormatting.DARK_GREEN)
        );
    }

}
