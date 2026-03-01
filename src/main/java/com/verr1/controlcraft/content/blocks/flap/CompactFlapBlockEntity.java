package com.verr1.controlcraft.content.blocks.flap;

import com.simibubi.create.AllSoundEvents;
import com.simibubi.create.content.contraptions.AbstractContraptionEntity;
import com.simibubi.create.content.contraptions.AssemblyException;
import com.simibubi.create.content.contraptions.bearing.BearingBlock;
import com.simibubi.create.content.contraptions.bearing.BearingContraption;
import com.simibubi.create.foundation.utility.Color;
import com.simibubi.create.foundation.utility.Couple;
import com.simibubi.create.foundation.utility.animation.LerpedFloat;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftClient;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.cctweaked.peripheral.CompactFlapPeripheral;
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
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.shared.Capabilities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Optional;

import static com.simibubi.create.content.kinetics.base.DirectionalAxisKineticBlock.AXIS_ALONG_FIRST_COORDINATE;
import static com.verr1.controlcraft.content.blocks.joints.AbstractJointBlock.FLIPPED;
import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;
import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class CompactFlapBlockEntity extends OnShipBlockEntity implements
        IReceiver, IPlant
{

    public SynchronizedField<Double> angle = new SynchronizedField<>(0.0);
    public SynchronizedField<Double> tilt  = new SynchronizedField<>(0.0);

    public static final NetworkKey ANGLE = NetworkKey.create("attack_angle");
    public static final NetworkKey TILT = NetworkKey.create("attack_tilt");
    public static final NetworkKey OFFSET = NetworkKey.create("angle_offset");
    public static final NetworkKey LIFT = NetworkKey.create("lift");
    public static final NetworkKey DRAG = NetworkKey.create("drag");
    public static final NetworkKey BIAS = NetworkKey.create("bias");
    public static final NetworkKey LEGACY = NetworkKey.create("legacy");
    public static final NetworkKey ASM = NetworkKey.create("c_asm");

    private final DirectReceiver receiver = new DirectReceiver();

    private final FlapPlant plant;

    private double offset = 0.0;



    private boolean legacyAeroDynamic = true;

    private double bias = 0.0;
    private double resistRatio = 30.0;
    private double liftRatio = 150.0;

    protected LerpedFloat clientAnimatedAngle = LerpedFloat.angular();
    protected LerpedFloat clientAnimatedTilt = LerpedFloat.angular();

    private CompactFlapPeripheral peripheral;
    private LazyOptional<IPeripheral> peripheralCap;



    private int clientContraptionId = 0;

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
                        this::angle,
                        this::setAngle,
                        SerializeUtils.DOUBLE
                ))
                .withClient(ClientBuffer.DOUBLE.get())
                .dispatchToSync()
                .register();

        buildRegistry(TILT)
                .withBasic(SerializePort.of(
                        this::tilt,
                        this::setTilt,
                        SerializeUtils.DOUBLE
                ))
                .withClient(ClientBuffer.DOUBLE.get())
                .dispatchToSync()
                .register();

        buildRegistry(LEGACY)
                .withBasic(SerializePort.of(
                        this::legacyAeroDynamic,
                        this::setLegacyAeroDynamic,
                        SerializeUtils.BOOLEAN
                ))
                .withClient(ClientBuffer.BOOLEAN.get())
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

        buildRegistry(ASM)
            .withBasic(SerializePort.of(
                this::clientContraptionId,
                this::setClientContraptionId,
                SerializeUtils.INT
            ))
            .dispatchToSync()
            .runtimeOnly()
            .register();


        panel().registerUnit(SharedKeys.ASSEMBLE, this::assemble);

        panel().registerUnit(SharedKeys.DISASSEMBLE, this::disassemble);

        receiver().register(
                new NumericField(
                        () -> angle.read(),
                        a -> {
                            angle.write(a);
                            queueUpdate(ANGLE);
                        },
                        "angle"
                ),
                new DirectReceiver.InitContext(SlotType.DEGREE, Couple.create(0.0, 1.0)),
                8
        ).register(
                new NumericField(
                        () -> tilt.read(),
                        a -> {
                            tilt.write(a);
                            queueUpdate(TILT);
                        },
                        "tilt"
                ),
                new DirectReceiver.InitContext(SlotType.TILT, Couple.create(0.0, 1.0)),
                8
        );
    }

    public int clientContraptionId() {
        if(level == null || level.isClientSide){
            return clientContraptionId;
        }
        return physicalWing == null ? -1 : physicalWing.getId();
    }

    public void setClientContraptionId(int clientContraptionId) {
        if(clientContraptionId == this.clientContraptionId)return;
        this.clientContraptionId = clientContraptionId;
        if(level != null && level.isClientSide){
            Entity e = level.getEntity(clientContraptionId);
            if(e instanceof FlapContraptionEntity flap){
                physicalWing = flap;
            }
        }
    }

    public boolean legacyAeroDynamic() {
        return legacyAeroDynamic;
    }

    public void setLegacyAeroDynamic(boolean legacyAeroDynamic) {
        this.legacyAeroDynamic = legacyAeroDynamic;
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

    private Vector3d getTiltAxis(){
        Direction tiltAxis = leftDirection();
        return ValkyrienSkies.set(new Vector3d(), tiltAxis.getNormal());
    }

    public Direction leftDirection() {
        BlockState state = getBlockState();
        if (state.hasProperty(AXIS_ALONG_FIRST_COORDINATE)) {
            Direction direction = getDirection();
            boolean alignFirst = state.getValue(AXIS_ALONG_FIRST_COORDINATE);

            boolean flipped = false;
            if (state.hasProperty(FLIPPED)) {
                flipped = state.getValue(FLIPPED);
            }

            Direction d0 = switch (direction) {
                case SOUTH, NORTH -> alignFirst ? Direction.WEST : Direction.UP;
                case EAST -> alignFirst ? Direction.UP : Direction.SOUTH;
                case WEST -> alignFirst ? Direction.UP : Direction.NORTH;
                case UP, DOWN -> alignFirst ? Direction.WEST : Direction.NORTH;
            };

            return flipped ? d0.getOpposite() : d0;

        } else {
            return left(getDirection());
        }

    }

    public static Direction left(Direction direction){
        if(direction.getAxis().isHorizontal()){
            return direction.getCounterClockWise(Direction.Axis.Y);
        }
        return direction.getCounterClockWise(Direction.Axis.Z);
    }

    private Vector3d getNormal(){
        Vector3d baseNormal = getBaseNormal();
        Vector3d rotateAxis = getRotateAxis();
        Vector3d tiltAxis = getTiltAxis();
        double radians0 = Math.toRadians(MathUtils.angleReset(this.angle.read() + offset));
        double radians1 = Math.toRadians(MathUtils.angleReset(this.tilt.read()));
        return baseNormal
                .rotateAxis((radians0), rotateAxis.x(), rotateAxis.y(), rotateAxis.z())
                .rotateAxis((radians1), tiltAxis.x(), tiltAxis.y(), tiltAxis.z());
    }

    public LogicalFlap getLogicalFlap(){
        return new LogicalFlap(
                getBlockPos(),
                getNormal(),
                liftRatio,
                resistRatio,
                legacyAeroDynamic
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
        queueUpdate(OFFSET);
    }

    public double getResistRatio() {
        return resistRatio;
    }

    public void setResistRatio(double resistRatio) {
        this.resistRatio = Math.abs(resistRatio);
    }

    public double getLiftRatio() {
        return liftRatio;
    }

    public void setLiftRatio(double liftRatio) {
        this.liftRatio = Math.abs(liftRatio);
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
        // syncForNear(true, ANGLE, OFFSET);
    }

    @Override
    public void tickCommon() {
        super.tickCommon();
        applyRotation();
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        queueUpdate(ASM);
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


    public double angle(){
        return angle.read();
    }

    public void setAngle(double angle){
        this.angle.write(angle);
        queueUpdate(ANGLE);
    }

    public double tilt(){
        return tilt.read();
    }

    public void setTilt(double tilt){
        this.tilt.write(tilt);
        queueUpdate(TILT);
    }

    @Override
    public String receiverName() {
        return "compact_flap";
    }


    @Override
    public void destroy() {
        if(level != null && !level.isClientSide){
            disassemble();
        }
        super.destroy();
    }

    @Override
    public void remove() {
        if(level != null && !level.isClientSide){
            disassemble();
        }
        super.remove();
    }
    // The following are bearing part


    public LerpedFloat getClientAnimatedAngle() {
        return clientAnimatedAngle;
    }

    public LerpedFloat getClientAnimatedTilt() {
        return clientAnimatedTilt;
    }

    private void tickAnimationData(){
        clientAnimatedAngle.chase(angle.read() + offset, 0.1, LerpedFloat.Chaser.EXP);
        clientAnimatedAngle.tickChaser();

        clientAnimatedTilt.chase(tilt.read(), 0.1, LerpedFloat.Chaser.EXP);
        clientAnimatedTilt.tickChaser();
    }

    private static double angleFix(Direction direction, double realAngle){
//        return direction.getAxisDirection() == Direction.AxisDirection.POSITIVE ?
//                realAngle  : -realAngle;
        return realAngle;
    }

    protected FlapContraptionEntity physicalWing;
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

        Direction direction = getDirection();
        Direction left = leftDirection();
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
        physicalWing = FlapContraptionEntity.create(level, this, wingContraption);
        BlockPos anchor = worldPosition.relative(direction);
        physicalWing.setPos(anchor.getX(), anchor.getY(), anchor.getZ());
        physicalWing.setAngleDirection(direction);
        physicalWing.setTiltDirection(left);
        level.addFreshEntity(physicalWing);

        AllSoundEvents.CONTRAPTION_ASSEMBLE.playOnServer(level, worldPosition);
        visualAngle = 0;
        queueUpdate(ASM);
        sendData();

    }



    public void disassemble() {
        if (!isAssembled()) return;
        visualAngle = 0;
        running = false;
        physicalWing.disassemble();
        AllSoundEvents.CONTRAPTION_DISASSEMBLE.playOnServer(level, worldPosition);
        physicalWing = null;
        queueUpdate(ASM);
        sendData();
    }

    protected void applyRotation() {
        if(level == null)return;
        if (physicalWing == null) return;
        float wingAngle = level.isClientSide ? clientAnimatedAngle.getValue() : (float) (angle.read().floatValue() + offset);
        float wingTilt = level.isClientSide ? clientAnimatedTilt.getValue() : tilt.read().floatValue();

        physicalWing.setAngle((float) angleFix(getDirection(), wingAngle));
        physicalWing.setTilt((float) angleFix(getDirection(), wingTilt));
        physicalWing.setAngleDirection(
            getDirection()
        );
        physicalWing.setTiltDirection(
            leftDirection()
        );
    }

    public boolean isAttachedTo(AbstractContraptionEntity contraption) {
        return contraption == physicalWing;
    }


    public void attach(FlapContraptionEntity contraption) {
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
