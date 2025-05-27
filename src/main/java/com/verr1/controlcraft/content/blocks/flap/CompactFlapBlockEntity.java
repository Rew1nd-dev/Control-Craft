package com.verr1.controlcraft.content.blocks.flap;

import com.simibubi.create.foundation.utility.Couple;
import com.simibubi.create.foundation.utility.animation.LerpedFloat;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.valkyrienskies.attachments.FlapForceInducer;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.NumericField;
import com.verr1.controlcraft.foundation.data.SynchronizedField;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.logical.LogicalFlap;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.redstone.DirectReceiver;
import com.verr1.controlcraft.foundation.redstone.IReceiver;
import com.verr1.controlcraft.foundation.type.descriptive.SlotType;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.api.ships.properties.ShipTransform;

import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;

public class CompactFlapBlockEntity extends OnShipBlockEntity implements
        IReceiver
{

    public SynchronizedField<Double> angle = new SynchronizedField<>(0.0);

    public static NetworkKey ANGLE = NetworkKey.create("attack_angle");

    private final DirectReceiver receiver = new DirectReceiver();



    private double offset = 0.0;
    private double resistRatio = 1.0;
    private double liftRatio = 1.0;

    private Vector3dc cachedRelative = new Vector3d(0, 0, 0);

    protected LerpedFloat clientAnimatedAngle = LerpedFloat.angular();


    public CompactFlapBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);

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
                .runtimeOnly()
                .register();

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

    @Override
    public DirectReceiver receiver() {
        return receiver;
    }

    private Vector3d getBaseNormal(){
        Direction dir = getDirection();
        if(dir == Direction.UP){
            return new Vector3d(1, 0, 0);
        }else if(dir == Direction.DOWN){
            return new Vector3d(-1, 0, 0);
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

    @Override
    public void tickServer() {
        super.tickServer();
        syncAttachInducer();
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        syncForNear(true, ANGLE);
    }

    @Override
    public void tickClient() {
        super.tickClient();
        tickAnimationData();
    }

    private void syncAttachInducer(){
        if(level == null || level.isClientSide)return;
        Optional
                .ofNullable(getLoadedServerShip())
                .map(FlapForceInducer::getOrCreate)
                .ifPresent(inducer -> inducer.replace(
                        WorldBlockPos.of(level, getBlockPos()),
                        this::getLogicalFlap
                ));
    }

    // currently don't use this, calculate relative in force inducer
    private void updateRelative(){
        cachedRelative = Optional
                .ofNullable(getLoadedServerShip())
                .map(Ship::getTransform)
                .map(ShipTransform::getPositionInShip)
                .map(v -> toJOML(getBlockPos().getCenter()).sub(v.add(0.5, 0.5, 0.5, new Vector3d())))
                .orElse(new Vector3d());
    }

    public Vector3dc getRelative() {
        return cachedRelative;
    }

    @Override
    public String name() {
        return "compact_flap";
    }


    public LerpedFloat getClientAnimatedAngle() {
        return clientAnimatedAngle;
    }

    private void tickAnimationData(){


        if(level == null || !level.isClientSide)return;
        clientAnimatedAngle.chase(angle.read(), 0.1, LerpedFloat.Chaser.EXP);
        clientAnimatedAngle.tickChaser();
    }
}
