package com.verr1.controlcraft.content.links;

import com.simibubi.create.content.equipment.goggles.IHaveGoggleInformation;
import com.verr1.controlcraft.content.blocks.NetworkBlockEntity;
import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ILinkableBlock;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.links.ConnectionStatus;
import com.verr1.controlcraft.foundation.data.links.ValueStatus;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.MinecraftUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;

public abstract class CimulinkBlockEntity<T extends BlockLinkPort> extends NetworkBlockEntity implements
        ILinkableBlock, IHaveGoggleInformation
{

    private T linkPort;

    @Override
    public final void initialize() {
        super.initialize();
        linkPort = create(level, getBlockPos());
        syncForAllPlayers(false, SharedKeys.CONNECTION_STATUS, SharedKeys.VALUE_STATUS);
        initializeExtra();
    }

    protected void initializeExtra(){

    }

    protected abstract T create(Level level, BlockPos pos);

    public CimulinkBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);

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
/*
buildRegistry(SharedKeys.STATUS)
                .withBasic(CompoundTagPort.of(
                        () -> ConnectionStatus.summarize(linkPort()),
                        $ -> {}
                ))
                .withClient(new ClientBuffer<>(
                        SerializeUtils.of(
                                $ -> new CompoundTag(),
                                ConnectionStatus::deserialize
                        ),
                        ConnectionStatus.class
                ))
                .dispatchToSync()
                .runtimeOnly();
* */
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
    private void requestValueStatusOnFocus(){
        BlockPos p = MinecraftUtils.lookingAtPos();
        if(p == null || !p.equals(getBlockPos()))return;
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
        linkPort().removeInvalid();
    }

    public ConnectionStatus readClientConnectionStatus(){
        return handler().readClientBuffer(SharedKeys.CONNECTION_STATUS, ConnectionStatus.class);
    }

    public ValueStatus readClientValueStatus(){
        return handler().readClientBuffer(SharedKeys.VALUE_STATUS, ValueStatus.class);
    }


    @Override
    public T linkPort(){
        return linkPort;
    }

    @Override
    public boolean addToGoggleTooltip(List<Component> tooltip, boolean isPlayerSneaking) {
        if(isPlayerSneaking){
            tooltip.addAll(makeDetailedToolTip(readClientConnectionStatus()));
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
                Component.literal("    values:"),
                inTitle,
                Component.literal(sbi.toString()).withStyle(ChatFormatting.GREEN),
                outTitle,
                Component.literal(sbo.toString()).withStyle(ChatFormatting.DARK_GREEN)
        );
    }

    public static List<Component> makeDetailedToolTip(ConnectionStatus cs){

        MutableComponent in = Component.literal("in: \n");
        cs.inputPorts.forEach((inName, outBp) -> {
            in.append(inName).append(" <- ").append(outBp.portName()).append("\n");
        });
        MutableComponent out = Component.literal("out: \n");
        cs.outputPorts.forEach((outName, inBps) -> {
            out.append(outName).append("->");
            MutableComponent bps = Component.literal("");
            inBps.forEach(bp -> bps.append(bp.portName()).append(", "));
            out.append(bps).append("\n");
        });
        return List.of(
                Component.literal("    Connection Status:").withStyle(ChatFormatting.GOLD),
                in.withStyle(ChatFormatting.GREEN),
                out.withStyle(ChatFormatting.DARK_GREEN)
        );
    }

}
