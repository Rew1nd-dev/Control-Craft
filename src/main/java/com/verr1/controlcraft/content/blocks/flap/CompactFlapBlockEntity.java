package com.verr1.controlcraft.content.blocks.flap;

import com.simibubi.create.AllSoundEvents;
import com.simibubi.create.content.contraptions.AbstractContraptionEntity;
import com.simibubi.create.content.contraptions.AssemblyException;
import com.simibubi.create.content.contraptions.ControlledContraptionEntity;
import com.simibubi.create.content.contraptions.bearing.BearingBlock;
import com.simibubi.create.content.contraptions.bearing.BearingContraption;
import com.simibubi.create.content.contraptions.bearing.IBearingBlockEntity;
import com.simibubi.create.foundation.utility.Color;
import com.simibubi.create.foundation.utility.Couple;
import com.simibubi.create.foundation.utility.animation.LerpedFloat;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftClient;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.cctweaked.peripheral.CompactFlapPeripheral;
import com.verr1.controlcraft.content.cctweaked.peripheral.FlapBearingPeripheral;
import com.verr1.controlcraft.content.valkyrienskies.attachments.FlapForceInducer;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.FlapPlant;
import com.verr1.controlcraft.foundation.data.*;
import com.verr1.controlcraft.foundation.data.logical.LogicalFlap;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.redstone.DirectReceiver;
import com.verr1.controlcraft.foundation.redstone.IReceiver;
import com.verr1.controlcraft.foundation.type.descriptive.SlotType;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.shared.Capabilities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.util.Mth;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Optional;

import static org.valkyrienskies.mod.api.ValkyrienSkies.toJOML;
import static org.valkyrienskies.mod.api.ValkyrienSkies.toMinecraft;

public class CompactFlapBlockEntity extends OnShipBlockEntity implements
        IReceiver, IBearingBlockEntity, IPlant
{

    public SynchronizedField<Double> angle = new SynchronizedField<>(0.0);

    public static final NetworkKey ANGLE = NetworkKey.create("attack_angle");
    public static final NetworkKey OFFSET = NetworkKey.create("angle_offset");
    public static final NetworkKey LIFT = NetworkKey.create("lift");
    public static final NetworkKey DRAG = NetworkKey.create("drag");
    public static final NetworkKey BIAS = NetworkKey.create("bias");

    public static final NetworkKey ADD_LEVEL = NetworkKey.create("add_flap_level");
    public static final NetworkKey DEC_LEVEL = NetworkKey.create("dec_flap_level");

    private final DirectReceiver receiver = new DirectReceiver();

    private final FlapPlant plant;

    private double offset = 0.0;

    private int flapLevel = 0;

    private double bias = 0.0;
    private double resistRatio = 3.0;
    private double liftRatio = 15.0;

    protected LerpedFloat clientAnimatedAngle = LerpedFloat.angular();

    private CompactFlapPeripheral peripheral;
    private LazyOptional<IPeripheral> peripheralCap;

    @Override
    public @NotNull <T> LazyOptional<T> getCapability(@NotNull Capability<T> cap, @Nullable Direction side) {
        if(cap == Capabilities.CAPABILITY_PERIPHERAL){
            if(this.peripheral == null){
                this.peripheral = new CompactFlapPeripheral(this);
            }
            if(peripheralCap == null || !peripheralCap.isPresent())
                peripheralCap = LazyOptional.of(() -> this.peripheral);
            return peripheralCap.cast();
        }
        return super.getCapability(cap, side);
    }

    public CompactFlapBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        plant = new FlapPlant(this);
        buildRegistry(FIELD)
                .withBasic(CompoundTagPort.of(
                        () -> receiver().serialize(),
                        t -> receiver().deserialize(t)
                ))
                .withClient(
                        new ClientBuffer<>(SerializeUtils.UNIT, CompoundTag.class)
                )
                .dispatchToSync()
                .register();

        buildRegistry(ANGLE)
                .withBasic(SerializePort.of(
                        () -> angle.read(),
                        a -> angle.write(a),
                        SerializeUtils.DOUBLE
                ))
                .withClient(ClientBuffer.DOUBLE.get())
                .dispatchToSync()
                .register();

        buildRegistry(OFFSET)
                .withBasic(SerializePort.of(
                        this::getOffset,
                        this::setOffset,
                        SerializeUtils.DOUBLE
                ))
                .withClient(ClientBuffer.DOUBLE.get())
                .dispatchToSync()
                .register();

        buildRegistry(LIFT)
                .withBasic(SerializePort.of(
                        this::getLiftRatio,
                        this::setLiftRatio,
                        SerializeUtils.DOUBLE
                ))
                .withClient(ClientBuffer.DOUBLE.get())
                .dispatchToSync()
                .register();

        buildRegistry(DRAG)
                .withBasic(SerializePort.of(
                        this::getResistRatio,
                        this::setResistRatio,
                        SerializeUtils.DOUBLE
                ))
                .withClient(ClientBuffer.DOUBLE.get())
                .dispatchToSync()
                .register();

        buildRegistry(BIAS)
                .withBasic(SerializePort.of(
                        this::getBias,
                        this::setBias,
                        SerializeUtils.DOUBLE
                ))
                .withClient(ClientBuffer.DOUBLE.get())
                .dispatchToSync()
                .register();

        panel().registerUnit(SharedKeys.ASSEMBLE, this::assemble);

        panel().registerUnit(SharedKeys.DISASSEMBLE, this::disassemble);

        panel().registerUnit(ADD_LEVEL, () -> setFlapLevel(getFlapLevel() + 1));

        panel().registerUnit(DEC_LEVEL, () -> setFlapLevel(getFlapLevel() - 1));

        receiver().register(
                new NumericField(
                        () -> angle.read(),
                        a -> angle.write(a),
                        "angle"
                ),
                new DirectReceiver.InitContext(SlotType.DEGREE, Couple.create(0.0, 1.0)),
                8
        );
    }



    public void setFlapLevel(int fLevel){
        if(fLevel <= 0)return;
        flapLevel = fLevel;
        setLiftRatio(fLevel * 15);
        setResistRatio(fLevel * 3);
    }

    public int getFlapLevel(){
        return flapLevel;
    }

    public void setAttackAngle(double angle){
        this.angle.write(angle);
    }

    @Override
    public DirectReceiver receiver() {
        return receiver;
    }

    private Vector3d getBaseNormal(){
        Direction dir = getDirection();
        if(dir == Direction.UP){
            return new Vector3d(0, 0, 1);
        }else if(dir == Direction.DOWN){
            return new Vector3d(0, 0, -1);
        }
        return new Vector3d(0, 1, 0);
    }

    private Vector3d getRotateAxis(){
        return getDirectionJOML();
    }

    private Vector3d getNormal(){
        Vector3d baseNormal = getBaseNormal();
        Vector3d rotateAxis = getRotateAxis();
        double radians = Math.toRadians(MathUtils.angleReset(this.angle.read() + offset));
        return baseNormal.rotateAxis((radians), rotateAxis.x(), rotateAxis.y(), rotateAxis.z());
    }

    public LogicalFlap getLogicalFlap(){
        return new LogicalFlap(
                getBlockPos(),
                getNormal(),
                liftRatio,
                resistRatio
        );
    }

    private void db_renderNormal(){
        Vector3dc n = getNormal();
        Vector3dc start = toJOML(getBlockPos().getCenter());
        Vector3dc end = start.fma(2, n, new Vector3d());

        ControlCraftClient.CLIENT_LERPED_OUTLINER.showLine(
                        "debug_flap_normal" + getBlockPos().asLong(),
                        toMinecraft(start),
                        toMinecraft(end)
                )
                .colored(Color.RED.setAlpha(0.6f))
                .lineWidth(0.3f);
    }

    public double getOffset() {
        return offset;
    }

    public void setOffset(double offset) {
        this.offset = offset;
    }

    public double getResistRatio() {
        return resistRatio;
    }

    public void setResistRatio(double resistRatio) {
        this.resistRatio = resistRatio;
    }

    public double getLiftRatio() {
        return liftRatio;
    }

    public void setLiftRatio(double liftRatio) {
        this.liftRatio = liftRatio;
    }

    public double getBias() {
        return bias;
    }

    public void setBias(double bias) {
        this.bias = bias;
    }

    @Override
    public void tickServer() {
        super.tickServer();
        syncAttachInducer();
        syncForNear(true, ANGLE, OFFSET);
    }

    @Override
    public void tickCommon() {
        super.tickCommon();
        applyRotation();
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        // syncForNear(true, ANGLE);
    }

    @Override
    public void tickClient() {
        super.tickClient();
        tickAnimationData();
        // db_renderNormal();
    }

    private void syncAttachInducer(){
        Optional
                .ofNullable(getLoadedServerShip())
                .map(FlapForceInducer::getOrCreate)
                .ifPresent(inducer -> inducer.replace(
                        WorldBlockPos.of(level, getBlockPos()),
                        this::getLogicalFlap
                ));
    }




    @Override
    public String name() {
        return "compact_flap";
    }


    @Override
    public void destroy() {
        if(level == null || !level.isClientSide){
            disassemble();
        }
        super.destroy();
    }

    @Override
    public void remove() {
        if(level == null || !level.isClientSide){
            disassemble();
        }
        super.remove();
    }
    // The following are bearing part


    public LerpedFloat getClientAnimatedAngle() {
        return clientAnimatedAngle;
    }

    private void tickAnimationData(){
        clientAnimatedAngle.chase(angle.read() + offset, 0.1, LerpedFloat.Chaser.EXP);
        clientAnimatedAngle.tickChaser();
    }

    private static double angleFix(Direction direction, double realAngle){
        return direction.getAxisDirection() == Direction.AxisDirection.POSITIVE ?
                realAngle  : -realAngle;
    }

    protected ControlledContraptionEntity physicalWing;
    protected float adjustSpeed;
    protected double visualAngle;
    protected boolean running;

    public boolean isAssembled(){
        return physicalWing != null;
    }

    public void assemble(){
        if(isAssembled())return;
        if (level == null || !(level.getBlockState(worldPosition).getBlock() instanceof BearingBlock))
            return;

        Direction direction = getBlockState().getValue(BearingBlock.FACING);
        WingContraption wingContraption = new WingContraption(direction);

        AssemblyException lastException;
        try {
            if (!wingContraption.assemble(level, worldPosition))
                return;

            lastException = null;
        } catch (AssemblyException e) {
            lastException = e;
            ControlCraft.LOGGER.info(e.toString());
            sendData();
            return;
        }

        running = true;
        wingContraption.removeBlocksFromWorld(level, BlockPos.ZERO);
        physicalWing = ControlledContraptionEntity.create(level, this, wingContraption);
        BlockPos anchor = worldPosition.relative(direction);
        physicalWing.setPos(anchor.getX(), anchor.getY(), anchor.getZ());
        physicalWing.setRotationAxis(direction.getAxis());
        level.addFreshEntity(physicalWing);

        AllSoundEvents.CONTRAPTION_ASSEMBLE.playOnServer(level, worldPosition);
        visualAngle = 0;
        sendData();

    }

    public void disassemble() {
        if (!isAssembled()) return;
        visualAngle = 0;
        running = false;
        physicalWing.disassemble();
        AllSoundEvents.CONTRAPTION_DISASSEMBLE.playOnServer(level, worldPosition);
        physicalWing = null;
        sendData();
    }

    protected void applyRotation() {
        if(level == null)return;
        float wingAngle = level.isClientSide ? clientAnimatedAngle.getValue() : angle.read().floatValue();
        if (physicalWing == null)
            return;
        physicalWing.setAngle((float) angleFix(getDirection(), wingAngle));
        BlockState blockState = getBlockState();
        if (blockState.hasProperty(BlockStateProperties.FACING))
            physicalWing.setRotationAxis(
                    blockState
                            .getValue(BlockStateProperties.FACING)
                            .getAxis()
            );
    }

    @Override
    public boolean isAttachedTo(AbstractContraptionEntity contraption) {
        return contraption == physicalWing;
    }

    @Override
    public void attach(ControlledContraptionEntity contraption) {
        BlockState blockState = getBlockState();
        if (!(contraption.getContraption() instanceof BearingContraption))
            return;
        if (!blockState.hasProperty(BearingBlock.FACING))
            return;

        this.physicalWing = contraption;
        setChanged();
        BlockPos anchor = worldPosition.relative(blockState.getValue(BearingBlock.FACING));
        physicalWing.setPos(anchor.getX(), anchor.getY(), anchor.getZ());
        if (level != null && !level.isClientSide) {
            sendData();
        }
    }

    @Override
    public void onStall() {
        if (level == null || !level.isClientSide)
            sendData();
    }

    @Override
    public boolean isValid() {
        return isRemoved();
    }

    @Override
    public BlockPos getBlockPosition() {
        return getBlockPos();
    }

    @Override
    public float getInterpolatedAngle(float partialTicks) {
        return (float) Mth.lerp(partialTicks, visualAngle, visualAngle + adjustSpeed * 0.05f);
    }

    @Override
    public boolean isWoodenTop() {
        return false;
    }

    @Override
    public void setAngle(float v) {
        setVisualAngle((double)v);
    }


    public void setVisualAngle(double forcedAngle) {
        visualAngle = MathUtils.angleReset((float) forcedAngle);
    }

    @Override
    public @NotNull NamedComponent plant() {
        return plant;
    }
}
