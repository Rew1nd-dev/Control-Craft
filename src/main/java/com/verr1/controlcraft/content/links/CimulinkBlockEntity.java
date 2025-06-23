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
        // renderer().tickSocketPositions();

    }



    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        if(linkPort() == null)return;
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
                        Component.literal("    [" + ConnectionStatus.mapToName(inBp.pos().pos(), world) + ":" + inBp.portName() + "]")
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






    /*
    public class Renderer{
    private final Map<String, RenderCurveKey> cachedKeys = new HashMap<>();
    ConnectionStatus cachedConnectionStatus = ConnectionStatus.EMPTY;

    ValueStatus cachedCurrentValueStatus = ValueStatus.EMPTY;
    ValueStatus cachedPreviousValueStatus = ValueStatus.EMPTY;



    final List<Vec3> socketPositions = Collections.synchronizedList(new ArrayList<>());

    void tickCached(){
        cachedConnectionStatus = Optional.ofNullable(readClientConnectionStatus()).orElse(ConnectionStatus.EMPTY);
        cachedPreviousValueStatus = cachedCurrentValueStatus;
        cachedCurrentValueStatus = Optional.ofNullable(readClientValueStatus()).orElse(ValueStatus.EMPTY);
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
        if(level ==null || !level.isClientSide)return;
        changedInput.stream().filter(i -> i < n())
                .map(cs().inputs::get)
                .map(cachedKeys::get)
                .map(ControlCraftClient.CLIENT_CURVE_OUTLINER::retrieveLine)
                .filter(CimulinkWireEntry.class::isInstance)
                .map(CimulinkWireEntry.class::cast)
                .forEach(CimulinkWireEntry::flash);
    }

    public Vec3 socketRenderOffset(){
        return toVec3(getDirection().getNormal()).scale(-0.4);
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

    void tick(){
        tickCached();
        tickBox();
        tickCurve();
        tickFlash();
    }

    @NotNull ConnectionStatus cs(){
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
        int total_mod = total % MAX_INPUTS_PER_COLUMN;

        int m_1 = count < total - total_mod ? MAX_INPUTS_PER_COLUMN : total_mod; // row count

        int mod = (count % MAX_INPUTS_PER_COLUMN);

        double dy = 1.0 / m_1;

        return (m_1 - 1) * dy / 2 - (mod * dy);
    }

    public Vec3 computeOutputPortOffset(int count){
        double x = deltaX(count, m());//0.25;
        double y = deltaY(count, m());
        Vec3 h = MinecraftUtils.toVec3(getHorizontal().getNormal());
        Vec3 v = MinecraftUtils.toVec3(getVertical().getNormal());
        return h.scale(x).add(v.scale(y));
    }

    public Vec3 computeInputPortOffset(int count){
        double x = -deltaX(count, m());//-0.25;
        double y =  deltaY(count, n());

        Vec3 h = MinecraftUtils.toVec3(getHorizontal().getNormal());
        Vec3 v = MinecraftUtils.toVec3(getVertical().getNormal());
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
                getBlockPos(),
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
                getBlockPos(),
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
        Ship s = getShipOn();
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
        return getBlockPos().getCenter();
    }

    public Direction facing(){
        return getDirection();
    }

    public void tickCurve(){
        cs().inputPorts.forEach((inName, value) -> {
            Vec3 inPosition = inPosition(inName);
            Direction inDir = facing();
            if(inPosition == null)return;
            BlockPos outPos = value.pos().pos();
            CimulinkBlockEntity<?>.Renderer ord = of(outPos);
            if(ord == null)return;
            String outName = value.portName();
            Direction outDir = ord.facing();
            Vec3 outPosition = ord.outPosition(outName);
            if(outPosition == null)return;

            BlockPort inPort = new BlockPort(WorldBlockPos.of(level, getBlockPos()), inName);
            BlockPort outPort = new BlockPort(WorldBlockPos.of(level, outPos), outName);

            var k = new RenderCurveKey(inPort, outPort, inPosition, outPosition, inDir, outDir);

            ControlCraftClient.CLIENT_CURVE_OUTLINER.showLine(k, k::createBezier);

            cachedKeys.put(inName, k);
        });
    }

    public void tickFlash(){
        flash(changedInput());
    }

    public static CimulinkBlockEntity<?>.Renderer of(BlockPos clientPos){
        return BlockEntityGetter.getLevelBlockEntityAt(
                        Minecraft.getInstance().level,
                        clientPos,
                        CimulinkBlockEntity.class
                )
                .map(CimulinkBlockEntity::renderer)
                .orElse(null);
    }

    public void tickBox() {
        if(!beingLookedAt())return;

        HitResult target = Minecraft.getInstance().hitResult;

        if (!(target instanceof BlockHitResult result) || level == null)
            return;

        if(result.getDirection() != facing())return;

        ClientViewContext cvc = computeContext(target.getLocation(), true);
        if(cvc == null)return;

        String portName = cvc.portName();
        ValueStatus vs = readClientValueStatus();

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

        ValueBox box = new ValueBox(label, bb, getBlockPos());
        var xy = computeLocalOffset(cvc);
        LinkPortSlot transform =
                (LinkPortSlot)new LinkPortSlot(
                        xy.getFirst() * 16,
                        xy.getSecond() * 16
                ).fromSide(getDirection()); //

        CreateClient.OUTLINER
                .showValueBox(getBlockPos(), box.transform(transform))
                .highlightFace(result.getDirection());



        List<MutableComponent> tip = new ArrayList<>();
        tip.add(inout.append(label).append(value));
        CreateClient.VALUE_SETTINGS_HANDLER.showHoverTip(tip);
    }

}
    * */


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

