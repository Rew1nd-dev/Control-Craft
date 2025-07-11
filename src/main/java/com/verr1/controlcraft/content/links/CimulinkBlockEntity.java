package com.verr1.controlcraft.content.links;

import com.simibubi.create.content.equipment.goggles.IHaveHoveringInformation;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.compact.vmod.VSchematicCompactCenter;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ILinkableBlock;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.links.*;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.MinecraftUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.fml.DistExecutor;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import static com.verr1.controlcraft.utils.MinecraftUtils.toVec3;

public abstract class CimulinkBlockEntity<T extends BlockLinkPort> extends OnShipBlockEntity implements
        ILinkableBlock, IHaveHoveringInformation
{

    private final T linkPort;
    // private final ClientWatcher watcher = new ClientWatcher();
    private boolean isInitialized = false;


    private IRenderer renderer;

    private final DeferralInitializer lateInitLinkPort = new DeferralInitializer() {
        @Override
        void deferralLoad(CompoundTag tag) {
            try{
                linkPort().deserialize(tag);
            }catch (NullPointerException e){
                ControlCraft.LOGGER.error("linkPort at: {} did not initialize linkPort!", getBlockPos().toShortString());
            }
        }
    };

    private final DeferralInitializer lateInitVModCompact = new DeferralInitializer() {
        @Override
        void deferralLoad(CompoundTag tag) {
            VSchematicCompactCenter.PostCimulinkReadVModCompact(CimulinkBlockEntity.this, tag);
        }
    };

    protected void initializeEarly(){
        setChanged();
    }

    @Override
    protected void readExtra(CompoundTag compound) {
        lateInitVModCompact.load(compound);
    }

    @Override
    public final void initializeServer() {
        super.initializeServer();

        initializeEarly();

        if(level != null){
            linkPort.setWorldBlockPos(WorldBlockPos.of(level, getBlockPos()));
        }else{
            ControlCraft.LOGGER.warn("link port has not been set pos! at: {} because level is null", getBlockPos().toShortString());
        }

        lateInitLinkPort.load(); // restore connections
        lateInitVModCompact.load(); // load with vmod compact (offset all links)
        initializeExtra();
        isInitialized = true;
        syncForAllPlayers(false, SharedKeys.CONNECTION_STATUS, SharedKeys.VALUE_STATUS);
        ControlCraft.LOGGER.info("be at {} finish initialization", getBlockPos().toShortString());
    }

    public boolean initialized(){
        return isInitialized;
    }


    public IRenderer renderer() {
        return renderer;
    }

    protected void initializeExtra(){

    }

    protected abstract T create();

    public CimulinkBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        linkPort = create();
        buildRegistry(SharedKeys.BLP)
                .withBasic(CompoundTagPort.of(
                        () -> linkPort().serialize(),
                        lateInitLinkPort::load
                ))
                .register();
        // this will be done in link ports serializations
        buildRegistry(SharedKeys.COMPONENT_NAME)
                .withBasic(SerializePort.of(
                        this::name,
                        this::setName,
                        SerializeUtils.STRING
                ))
                .runtimeOnly()
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


        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> renderer = new CimulinkRenderer(this));

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
        // if(p == null || !p.equals(getBlockPos()))return;
        handler().request(SharedKeys.CONNECTION_STATUS);
    }

    @OnlyIn(Dist.CLIENT)
    boolean beingLookedAt(){
        BlockPos p = MinecraftUtils.lookingAtPos();
        return p != null && p.equals(getBlockPos());
    }

    @OnlyIn(Dist.CLIENT)
    private void requestValueStatusOnFocus(){
        handler().request(SharedKeys.VALUE_STATUS);
    }

    @Override
    public void tickClient() {
        super.tickClient();
        requestValueStatusOnFocus();
        renderer().tick();
    }

    @Override
    public void removeServer() {
        super.removeServer();
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
        syncForNear(false, SharedKeys.COMPONENT_NAME);
    }

    public @Nullable ConnectionStatus readClientConnectionStatus(){
        return handler().readClientBuffer(SharedKeys.CONNECTION_STATUS, ConnectionStatus.class);
    }

    public boolean isValueStatusDirty(){
        return handler().isAnyDirty(SharedKeys.VALUE_STATUS);
    }

    public boolean isConnectionStatusDirty(){
        return handler().isAnyDirty(SharedKeys.VALUE_STATUS);
    }

    public void setValueStatueDirty(){
        handler().setDirty(SharedKeys.VALUE_STATUS);
    }

    public void setConnectionStatusDirty(){
        handler().setDirty(SharedKeys.CONNECTION_STATUS);
    }

    public @Nullable  ValueStatus readClientValueStatus(){
        return handler().readClientBuffer(SharedKeys.VALUE_STATUS, ValueStatus.class);
    }

    public String readClientComponentName(){
        return handler().readClientBuffer(SharedKeys.COMPONENT_NAME, String.class);
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
        return Objects.requireNonNull(linkPort);
    }



    @Override
    public boolean addToTooltip(List<Component> tooltip, boolean isPlayerSneaking) {
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
        MutableComponent in = Component.literal("in:").withStyle(s -> s.withColor(ChatFormatting.BLUE).withBold(true));
        List<Component> inputComponents = new ArrayList<>();

        cs.inputPorts.forEach((inName, outBp) -> inputComponents.add(
                Component.literal("  " + inName + " <-").withStyle(s -> s.withColor(ChatFormatting.BLUE))
                        .append(
                Component.literal("[" +  ConnectionStatus.mapToName(outBp.pos().pos(), world) + ":" + outBp.portName() + "]")
                        .withStyle(s -> s.withColor(ChatFormatting.DARK_AQUA).withUnderlined(true))
                        )

        ));

        MutableComponent out = Component.literal("out: ").withStyle(s -> s.withColor(ChatFormatting.RED).withBold(true));
        List<Component> outputComponents = new ArrayList<>();
        cs.outputPorts.forEach((outName, inBps) -> {

            outputComponents.add(Component.literal("  " + outName + "->").withStyle(s -> s.withColor(ChatFormatting.RED)));
            inBps.forEach(inBp ->
                outputComponents.add(
                        Component.literal("[" + ConnectionStatus.mapToName(inBp.pos().pos(), world) + ":" + inBp.portName() + "]")
                                .withStyle(s -> s.withColor(ChatFormatting.DARK_AQUA).withUnderlined(true))
                )
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


    protected abstract static class DeferralInitializer{

        CompoundTag savedTag = new CompoundTag();

        void load(CompoundTag savedTag){
            this.savedTag = savedTag;
        }

        void load(){
            deferralLoad(savedTag);
        }

        abstract void deferralLoad(CompoundTag tag);
    }





}



// TODO:
// 目前：
// ConnectionStatus: 保存自己和谁连接，被谁连了 String -> BlockPort, index -> String
// ValueStatus：端口的值，index -> double
// RenderCenter： 计算ValueBox，渲染端口
// Curve：需要输出端的位置

// 一个ClientSide的RenderManager：(inner class)
// 请求同步cs,vs
// 类似服务端BlockLinkPort::of，通过BlockPos获取其他cbe的RenderManager
// 根据cs,vs,获取各种反查函数，如name->index index->name, index->vec3....
// 管理ValueBox，动态改变其offset，根据cs和vs
// 监听vs变化，
// 生成渲染用的BezierCurveEntry
// 计算客户端玩家正在看着哪个端口

