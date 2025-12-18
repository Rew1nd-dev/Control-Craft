package com.verr1.controlcraft.content.blocks.jet;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.api.IPacketHandler;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundClientPacket;
import com.verr1.controlcraft.foundation.type.RegisteredPacketType;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import com.verr1.controlcraft.utils.LazyField;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.VSMathUtils;
import net.createmod.catnip.animation.LerpedFloat;
import net.createmod.catnip.data.Couple;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.PacketDistributor;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.mod.api.ValkyrienSkies;
import org.valkyrienskies.mod.common.VSGameUtilsKt;

public class JetRudderBlockEntity extends OnShipBlockEntity implements
        IPacketHandler
{

    public static NetworkKey THRUST = NetworkKey.create("thrust");
    public static NetworkKey HORIZONTAL = NetworkKey.create("horizontal");
    public static NetworkKey VERTICAL = NetworkKey.create("vertical");

    public LerpedFloat animatedHorizontalAngle = LerpedFloat.angular();

    private Vector3dc lastTickDirection = new Vector3d();
    private Vector3dc thisTickDirection = new Vector3d();



    public LerpedFloat animatedVerticalAngle = LerpedFloat.angular();

    public LazyField<Float> targetHorizontalAngle = new LazyField<>(0.0f, LazyField.FLOAT_COMP.apply(1e-3));
    public LazyField<Float> targetVerticalAngle = new LazyField<>(0.0f, LazyField.FLOAT_COMP.apply(1e-3));
    public LazyField<Float> targetThrust = new LazyField<>(0.0f, LazyField.FLOAT_COMP.apply(1e-3));

    private Direction vertical = Direction.UP;
    private Direction horizontal = Direction.NORTH;

    public float getTargetThrust() {
        return targetThrust.get();
    }

    public float getTargetVerticalAngle() {
        return targetVerticalAngle.get();
    }

    public float getTargetHorizontalAngle() {
        return targetHorizontalAngle.get();
    }

    public void setTargetHorizontalAngle(float targetHorizontalAngle) {
        this.targetHorizontalAngle.set((float) VSMathUtils.clamp(targetHorizontalAngle, Math.toRadians(90)));
        if(this.targetHorizontalAngle.pollDirty())queueUpdate(HORIZONTAL);
    }

    public void setTargetVerticalAngle(float targetVerticalAngle) {
        this.targetVerticalAngle.set((float) VSMathUtils.clamp(targetVerticalAngle, Math.toRadians(90)));
        if(this.targetVerticalAngle.pollDirty())queueUpdate(VERTICAL);
    }

    public void setTargetThrust(float targetThrust) {
        this.targetThrust.set(targetThrust);
        if(this.targetThrust.pollDirty())queueUpdate(THRUST);
    }

    public Direction getFiexdDirection() {
        return getDirection().getOpposite();
    }


    public void updateVertical(){
        // if(!(level instanceof ServerLevel lvl))return Direction.UP;
        vertical = BlockEntityGetter.getLevelBlockEntityAt(level, getBlockPos().relative(getDirection().getOpposite()), JetBlockEntity.class)
                .map(JetBlockEntity::getVertical).orElse(Direction.UP);
    }

    // attacker rudder's direction is the opposite of attacker
    public void updateHorizontal(){
        // if(!(level instanceof ServerLevel lvl))return Direction.NORTH;
        horizontal = BlockEntityGetter.getLevelBlockEntityAt(level, getBlockPos().relative(getDirection().getOpposite()), JetBlockEntity.class)
                .map(JetBlockEntity::getHorizontal).orElse(Direction.NORTH);
    }

    public Direction getVertical(){
        // if(!(level instanceof ServerLevel lvl))return Direction.UP;
        return vertical;
    }

    // attacker rudder's direction is the opposite of attacker
    public Direction getHorizontal(){
        // if(!(level instanceof ServerLevel lvl))return Direction.NORTH;
        return horizontal;
    }

    public Vector3d getVerticalJOML(){
        return ValkyrienSkies.set(new Vector3d(), getVertical().getNormal());
    }

    public Vector3d getHorizontalJOML(){
        return ValkyrienSkies.set(new Vector3d(), getHorizontal().getNormal());
    }

    public JetRudderBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        /*
        registerFieldReadWriter(SerializeUtils.ReadWriter.of(this::getTargetThrust, this::setTargetThrust, SerializeUtils.FLOAT, THRUST), Side.RUNTIME_SHARED);
        registerFieldReadWriter(SerializeUtils.ReadWriter.of(this::getTargetHorizontalAngle, this::setTargetHorizontalAngle, SerializeUtils.FLOAT, HORIZONTAL), Side.RUNTIME_SHARED);
        registerFieldReadWriter(SerializeUtils.ReadWriter.of(this::getTargetVerticalAngle, this::setTargetVerticalAngle, SerializeUtils.FLOAT, VERTICAL), Side.RUNTIME_SHARED);

        * */
        buildRegistry(THRUST).withBasic(SerializePort.of(this::getTargetThrust, this::setTargetThrust, SerializeUtils.FLOAT)).dispatchToSync().runtimeOnly().constantSync().register();
        buildRegistry(HORIZONTAL).withBasic(SerializePort.of(this::getTargetHorizontalAngle, this::setTargetHorizontalAngle, SerializeUtils.FLOAT)).dispatchToSync().runtimeOnly().constantSync().register();
        buildRegistry(VERTICAL).withBasic(SerializePort.of(this::getTargetVerticalAngle, this::setTargetVerticalAngle, SerializeUtils.FLOAT)).dispatchToSync().runtimeOnly().constantSync().register();
    }

    public void setAnimatedAngles(double horizontal, double vertical, double thrust){
        setTargetThrust((float) thrust);
        setTargetHorizontalAngle((float) horizontal);
        setTargetVerticalAngle((float) vertical);
    }


    public Couple<Double> getRenderAngles(){

        int sign_fix = getDirection() == Direction.SOUTH || getDirection() == Direction.EAST || getDirection() == Direction.UP ? -1 : 1;
        float h = animatedHorizontalAngle.getValue(1) * sign_fix;
        float v = animatedVerticalAngle.getValue(1);

        double sh = Math.sin(h) / Math.sqrt(2);
        double sv = Math.sin(v) / Math.sqrt(2);
        double st = Math.sqrt(Math.abs(1 - (sh * sh + sv * sv))); // in case of < 0
        double rh = Math.atan2(sh, st);
        double rv = Math.atan2(sv, st);

        return Couple.create(rh, rv);
    }

    private void tickDirections(){
        lastTickDirection = thisTickDirection;
        thisTickDirection = getRenderThrustDir();
    }

    public Vector3dc getRenderDirection(float partialTick){
        return lastTickDirection.lerp(thisTickDirection, partialTick, new Vector3d()).normalize();
    }

    @Override
    public void tickClient() {
        super.tickClient();
        tickAnimation();
        tickParticles();
        tickDirections();
    }

    @Override
    public void lazyTickClient() {
        super.lazyTickClient();
        updateHorizontal();
        updateVertical();
    }

    @Override
    public void tickServer() {
        super.tickServer();
        // syncForNear(true, THRUST, HORIZONTAL, VERTICAL);
        // syncClient();
    }


    @Override
    public void lazyTick() {
        super.lazyTick();
        checkDrivenJet();
    }

    public void checkDrivenJet(){
        if(level == null || level.isClientSide)return;
        BlockPos jetPos = getBlockPos().relative(getDirection().getOpposite());
        if(!(level.getExistingBlockEntity(jetPos) instanceof JetBlockEntity jet)){
            setAnimatedAngles(0, 0, 0);
        }
    }


    public void tickParticles(){
        if(level == null || !level.isClientSide)return;
        Ship ship = VSGameUtilsKt.getShipObjectManagingPos(level, getBlockPos());


        Vector3d dir = getRenderThrustDir().mul(-1);

        Vector3d p_wc = ValkyrienSkies.set(new Vector3d(), getBlockPos().getCenter()).fma(0.2, getDirectionJOML());
        Vector3d v_wc = dir.mul(MathUtils.clamp1(targetThrust.get() * 1e-3) * 3, new Vector3d());

        if(v_wc.lengthSquared() < 1e-2)return;

        Vector3d extraVelocity = new Vector3d();

        if(ship != null){
            ship.getTransform().getShipToWorld().transformPosition(p_wc);
            ship.getTransform().getShipToWorld().transformDirection(v_wc);
            Vector3d r_sc = ValkyrienSkies.set(new Vector3d(), getBlockPos().relative(getDirection())).sub(ship.getTransform().getPositionInShip());
            Vector3d r_wc = ship.getTransform().getShipToWorld().transformDirection(r_sc);
            extraVelocity = ship.getOmega().cross(r_wc, new Vector3d()).add(ship.getVelocity());
        }
        v_wc.add(extraVelocity.mul(0.05));

        addParticles(p_wc, v_wc);

    }

    private void addParticles(Vector3dc p_wc, Vector3dc v_wc){
        if(level == null)return;
        //if(v_wc.lengthSquared() < 0.1)return;
        double scale = v_wc.length();
        Vector3d dir = v_wc.normalize(new Vector3d()).mul(scale);
        double spread = 0.1;
        level.addParticle(
                ParticleTypes.CLOUD,
                p_wc.x() + (2 * level.random.nextDouble() - 1) * spread,
                p_wc.y() + (2 * level.random.nextDouble() - 1) * spread,
                p_wc.z() + (2 * level.random.nextDouble() - 1) * spread,
                dir.x(),
                dir.y(),
                dir.z()
        );

    }



    private Vector3d getRenderThrustDir() {
        Vector3dc basis_h = getHorizontalJOML();
        Vector3dc basis_v = getVerticalJOML();
        Vector3dc basis_t = getDirectionJOML().mul(-1);  // make it the opposite (set to bounded attacker direction)

        float h = targetHorizontalAngle.get();
        float v = targetVerticalAngle.get();

        return JetBlockEntity.getThrustDir(h, v, basis_h, basis_v, basis_t);
    }

    @OnlyIn(Dist.CLIENT)
    private void tickAnimation(){
        animatedHorizontalAngle.chase(targetHorizontalAngle.get() , 0.1, LerpedFloat.Chaser.EXP);
        animatedVerticalAngle.chase(targetVerticalAngle.get() , 0.1, LerpedFloat.Chaser.EXP);
        animatedHorizontalAngle.tickChaser();
        animatedVerticalAngle.tickChaser();
    }



}
