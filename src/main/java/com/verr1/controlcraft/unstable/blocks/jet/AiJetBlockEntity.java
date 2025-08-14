package com.verr1.controlcraft.unstable.blocks.jet;

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
import com.verr1.controlcraft.unstable.ai.game.cruiser.v1.DubinsHolderV2;
import com.verr1.controlcraft.unstable.ai.game.jet.MakePathAction;
import com.verr1.controlcraft.unstable.ai.game.jet.PathAlongAction;
import com.verr1.controlcraft.unstable.pathing.path.IPath;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.ConstantCruiseNavigator;
import com.verr1.controlcraft.unstable.valkyrienskies.context.CruiserControllerV4;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalDirectionTarget;
import com.verr1.controlcraft.unstable.valkyrienskies.context.PoseController;
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

import java.awt.*;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class AiJetBlockEntity extends OnShipBlockEntity {
    public static final NetworkKey GOAL = NetworkKey.create("cruiser_goal");
    public static final NetworkKey PATH = NetworkKey.create("cruiser_path");


    public static final NetworkKey VEL = NetworkKey.create("cruiser_vel");
    public static final NetworkKey RAD = NetworkKey.create("cruiser_rad");
    public static final NetworkKey TAR = NetworkKey.create("db_tar");
    public static final NetworkKey TOL = NetworkKey.create("shoot_tol");
    public static final NetworkKey TWI = NetworkKey.create("cruise_twist");
    public static final NetworkKey YAW = NetworkKey.create("cruise_yaw");


    public static Address<AiJetBlockEntity> CONTEXT = new Address<>("cruiser", AiJetBlockEntity.class);
    public static Address<IPath> CURRENT_CRUISE = new Address<>("cruiser_current_cruise", IPath.class);

    private final Blackboard storage = new Blackboard();
    private final BehaviorTree ai;
    private final CruiserControllerV4 cruiseController = new CruiserControllerV4();


    private final PoseController poseController = new PoseController();


    private String debugTargetName = "@Target";

    private double cruiseVelocity = 50;
    private double shootTolerance = 7.5;



    private double twistOmega = 7;

    public AiJetBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
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

        buildRegistry(YAW)
                .withBasic(SerializePort.of(this::yawOmega, this::setYawOmega, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        storage.set(CONTEXT, this);

        Node root = new Parallel(ParallelPolicy.SUCCEED_ON_ALL).addChild(
                new Always(
                        new Sequence().addChild(
                                new MakePathAction(),
                                new PathAlongAction()
                        )
                )
        );


        ai = new BehaviorTree(root);

    }

    public double yawOmega(){
        return controller().yawOmega();
    }

    public void setYawOmega(double yawOmega){
        controller().setYawOmega(yawOmega);
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
        this.controller().setVelocity(cruiseVelocity);
    }

    public double cruiseRadius(){
        return controller().radius();
    }

    public void setCruiseRadius(double radius){
        controller().setRadius(radius);
    }


    public void debug_renderPathUnsafe(){
        IPath path = storage.get(CURRENT_CRUISE);
        if(path == null)return;
        double delta = path.length() / 30;
        for(int i = 0; i < 30; i++){
            Vector3dc p = path.point(i * delta);
            ClientOutliner.drawOutline(
                    toMinecraft(MathUtils.centerWithRadius(p, 1)),
                    Color.RED.getRGB(),
                    "debug_path" + getBlockPos() + i,
                    1,
                    1f / 16
            );
        }

    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        // debug_renderPathUnsafe();
    }

    public LogicalDirectionTarget getLogical(){
        return new LogicalDirectionTarget(cruiseController, poseController, false);
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
