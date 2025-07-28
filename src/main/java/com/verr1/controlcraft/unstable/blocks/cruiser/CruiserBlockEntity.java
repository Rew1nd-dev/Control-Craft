package com.verr1.controlcraft.unstable.blocks.cruiser;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.content.blocks.spinalyzer.SpinalyzerBlockEntity;
import com.verr1.controlcraft.content.cctweaked.peripheral.SpinalyzerPeripheral;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.managers.ClientOutliner;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.BehaviorTree;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.*;
import com.verr1.controlcraft.unstable.ai.game.cruiser.*;
import com.verr1.controlcraft.unstable.ai.game.cruiser.v1.ChaseAction;
import com.verr1.controlcraft.unstable.ai.game.cruiser.v1.DubinsHolderV2;
import com.verr1.controlcraft.unstable.ai.game.cruiser.v1.EscapeAction;
import com.verr1.controlcraft.unstable.ai.game.cruiser.v1.Situation;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.ConstantCruiseNavigator;
import com.verr1.controlcraft.unstable.valkyrienskies.context.*;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import rbasamoyai.createbigcannons.index.CBCEntityTypes;
import rbasamoyai.createbigcannons.munitions.autocannon.ap_round.APAutocannonProjectile;

import java.awt.*;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class CruiserBlockEntity extends OnShipBlockEntity {
    public static final NetworkKey GOAL = NetworkKey.create("cruiser_goal");
    public static final NetworkKey PATH = NetworkKey.create("cruiser_path");


    public static final NetworkKey VEL = NetworkKey.create("cruiser_vel");
    public static final NetworkKey RAD = NetworkKey.create("cruiser_rad");
    public static final NetworkKey TAR = NetworkKey.create("db_tar");
    public static final NetworkKey TOL = NetworkKey.create("shoot_tol");
    public static final NetworkKey TWI = NetworkKey.create("cruise_twist");
    public static final Address<Situation> AWARENESS = new Address<>("awareness", Situation.class);


    public static Address<CruiserBlockEntity> CONTEXT = new Address<>("cruiser", CruiserBlockEntity.class);
    private final Blackboard storage = new Blackboard();
    private final BehaviorTree ai;
    private final CruiserControllerV4 cruiseController = new CruiserControllerV4();
    private final Situation awareness = new Situation(this);


    private final PoseController poseController = new PoseController();


    private String debugTargetName = "@Target";

    private double cruiseVelocity = 50;
    private double shootTolerance = 7.5;



    private double twistOmega = 7;

    public CruiserBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);



        buildRegistry(VEL)
                .withBasic(SerializePort.of(this::cruiseVelocity, this::setCruiseVelocity, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(RAD)
                .withBasic(SerializePort.of(this::cruiseRadius, this::setCruiseRadius, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(TOL)
                .withBasic(SerializePort.of(this::shootTolerance, this::setShootTolerance, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(TAR)
                .withBasic(SerializePort.of(this::debugTargetName, this::setDebugTargetName, SerializeUtils.STRING))
                .withClient(ClientBuffer.STRING.get())
                .register();

        buildRegistry(TWI)
                .withBasic(SerializePort.of(this::twistOmega, this::setTwistOmega, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        storage.set(CONTEXT, this);
        storage.set(AWARENESS, awareness);

        Node root = new Parallel(ParallelPolicy.SUCCEED_ON_ALL).addChild(
                new Always(
                        new Selector().addChild(
                                new PivotToAction(),
                                new PivotAwayAction())
                        ),
                // new PivotToAction(),
                // new PivotAwayAction(),
                new FireAction(),
                new AwarenessAction()
        );


        ai = new BehaviorTree(root);

    }

    public double twistOmega() {
        // return twistOmega;
        return controller().twistOmega();
    }

    public void setTwistOmega(double twistOmega) {
        // this.twistOmega = twistOmega;
        controller().setTwistOmega(twistOmega);
    }

    public String debugTargetName() {
        return debugTargetName;
    }

    public void setDebugTargetName(String debugTargetName) {
        this.debugTargetName = debugTargetName;
    }

    public double shootTolerance() {
        return shootTolerance;
    }

    public void setShootTolerance(double shootTolerance) {
        this.shootTolerance = shootTolerance;
    }

    public double cruiseVelocity() {
        return cruiseVelocity;
    }

    public void setCruiseVelocity(double cruiseVelocity) {
        this.cruiseVelocity = cruiseVelocity;
    }

    public double cruiseRadius(){
        return controller().radius();
    }

    public void setCruiseRadius(double radius){
        controller().setRadius(radius);
    }



    public LogicalDirectionTarget getLogical(){
        return new LogicalDirectionTarget(cruiseController, poseController);
    }

    public PoseController poseController() {
        return poseController;
    }

    public void syncCruiseTarget(){
        if(level == null || level.isClientSide) return;
        Optional
                .ofNullable(getLoadedServerShip())
                .map(ConstantCruiseNavigator::getOrCreate)
                .ifPresent(navi -> navi.replace(
                        WorldBlockPos.of(level, getBlockPos()),
                        this::getLogical
                ));
    }

    public @NotNull CruiserControllerV4 controller(){
        return cruiseController;
    }

    public void fireAt(Vector3dc direction){
        if(isClientSide())return;
        Objects.requireNonNull(level);
        Vector3dc p = readSelf().position();
        Vector3dc front = readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1));
        Vector3dc spawn = p.fma(10.0, front, new Vector3d());
        APAutocannonProjectile ap = new APAutocannonProjectile(CBCEntityTypes.AP_AUTOCANNON.get(), level);
        // Arrow ap = new Arrow(EntityType.ARROW, level);
        ap.setPos(toMinecraft(spawn));
        ap.setTracer(true);
        ap.setChargePower(8);
        ap.setLifetime(40);
        ap.shoot(direction.x(), direction.y(), direction.z(), 9, 0);
        level.addFreshEntity(ap);
    }

    public @NotNull Vector3dc getPosition(){
        return readSelf().position();
    }

    public @NotNull Vector3dc getVelocity(){
        return readSelf().velocity();
    }

    public Vector3dc getHeading(){
        return readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1)); // assuming is facing +z
    }

    private void tickAI(){
        ai.update(storage);
    }

    public @Nullable Vector3dc debug_getTarget(){
        WorldBlockPos wpb = ControlCraftServer.CC_NETWORK.valid(new PeripheralNetwork.PeripheralKey(0L, debugTargetName));
        if(wpb == null)return null;
        return BlockEntityGetter.INSTANCE.getBlockEntityAt(wpb, PeripheralInterfaceBlockEntity.class)
                .map(PeripheralInterfaceBlockEntity::attachedPeripheral)
                .filter(SpinalyzerPeripheral.class::isInstance)
                .map(SpinalyzerPeripheral.class::cast)
                .map(SpinalyzerPeripheral::getTarget)
                .map(SpinalyzerBlockEntity::getPosition)
                .orElse(null);
    }

    public @Nullable Vector3dc debug_getTargetVelocity(){
        WorldBlockPos wpb = ControlCraftServer.CC_NETWORK.valid(new PeripheralNetwork.PeripheralKey(0L, debugTargetName));
        if(wpb == null)return null;
        return BlockEntityGetter.INSTANCE.getBlockEntityAt(wpb, PeripheralInterfaceBlockEntity.class)
                .map(PeripheralInterfaceBlockEntity::attachedPeripheral)
                .filter(SpinalyzerPeripheral.class::isInstance)
                .map(SpinalyzerPeripheral.class::cast)
                .map(SpinalyzerPeripheral::getTarget)
                .map(SpinalyzerBlockEntity::getVelocity)
                .orElse(null);
    }

    private Vector3dc readClientGoal(){
        return handler().readClientBuffer(GOAL, Vector3dc.class);
    }

    private DubinsHolderV2 readClientPath(){
        return handler().readClientBuffer(PATH, DubinsHolderV2.class);
    }

    @OnlyIn(Dist.CLIENT)
    private void debug_renderGoal(){
        Vector3dc goal = readClientGoal();
        if(goal == null)return;
        ClientOutliner.drawOutline(toMinecraft(MathUtils.centerWithRadius(goal, 0.5)), Color.GREEN.getRGB(), getBlockPos().toShortString(), 0.4, 1f / 16);
    }

    @OnlyIn(Dist.CLIENT)
    private void debug_renderPath(){
        DubinsHolderV2 goal = readClientPath();
        if(goal == null)return;
        AtomicInteger i = new AtomicInteger(0);
        goal.asList().forEach(v -> {
            ClientOutliner.drawOutline(toMinecraft(MathUtils.centerWithRadius(v, 0.5)), Color.RED.getRGB(),  getBlockPos().toShortString() + "_" + i.getAndIncrement(), 0.4, 1f / 16);
            // ClientOutliner.drawOutline(BlockPos.containing(toMinecraft(v)), Color.GREEN.getRGB(),  getBlockPos().toShortString() + "_" + i.getAndIncrement());
        });

    }

    @Override
    public void tickClient() {
        super.tickClient();
        debug_renderGoal();
        debug_renderPath();
    }

    @Override
    public void tickServer() {
        super.tickServer();
        tickAI();
        syncForAllPlayers(false, GOAL, PATH);
        syncCruiseTarget();
    }
}
