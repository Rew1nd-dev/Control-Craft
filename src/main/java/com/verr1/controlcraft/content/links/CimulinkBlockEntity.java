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

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import static com.verr1.controlcraft.utils.MinecraftUtils.toVec3;

public abstract class CimulinkBlockEntity<T extends BlockLinkPort> extends NetworkBlockEntity implements
        ILinkableBlock, IHaveGoggleInformation
{

    private final T linkPort;

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

    public Vec3 getFaceCenter(){
        Vec3 faceDir = toVec3(getDirection().getNormal());
        return getBlockPos().getCenter().add(faceDir.scale(-0.2));
    }

    public void setName(String name){
        linkPort().setName(name);
    }

    public String name(){
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

        List<Component> inputComponents = new ArrayList<>();
        IntStream.range(0, inSize).forEach(
                i -> inputComponents.add(Component.literal(cs.inputs.get(i) + ": [" + "%.2f".formatted(vs.inputValues.get(i)) + "]"))
        );
        List<Component> outputComponents = new ArrayList<>();
        IntStream.range(0, outSize).forEach(
                i -> outputComponents.add(Component.literal(cs.outputs.get(i) + ": [" + "%.2f".formatted(vs.outputValues.get(i)) + "]"))
        );

        ArrayList<Component> result = new ArrayList<>();
        result.add(Component.literal("    Values:").withStyle(ChatFormatting.GOLD));
        result.add(inTitle);
        result.addAll(inputComponents);
        result.add(outTitle);
        result.addAll(outputComponents);


        return result;
    }

    public static List<Component> makeDetailedToolTip(ConnectionStatus cs, Level world){
        if(cs == null)return List.of();
        MutableComponent in = Component.literal("in:");
        List<Component> inputComponents = new ArrayList<>();

        cs.inputPorts.forEach((inName, outBp) -> inputComponents.add(Component.literal("    " + inName + "<-" + ConnectionStatus.mapToName(outBp.pos().pos(), world) + "-" + outBp.portName())));

        MutableComponent out = Component.literal("out: ");
        List<Component> outputComponents = new ArrayList<>();
        cs.outputPorts.forEach((outName, inBps) -> {

            outputComponents.add(Component.literal(outName + "->"));
            inBps.forEach(inBp ->
                outputComponents.add(Component.literal("    " + ConnectionStatus.mapToName(inBp.pos().pos(), world) + "-" + inBp.portName()))
            );
        });

        ArrayList<Component> result = new ArrayList<>();
        result.add(Component.literal("    Connection Status").withStyle(ChatFormatting.GOLD));
        result.add(in);
        result.addAll(inputComponents);
        result.add(out);
        result.addAll(outputComponents);
        return result;
    }

}
