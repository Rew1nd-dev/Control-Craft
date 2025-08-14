package com.verr1.controlcraft.unstable.blocks.cruiser;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.content.blocks.spinalyzer.SpinalyzerBlockEntity;
import com.verr1.controlcraft.content.cctweaked.peripheral.SpinalyzerPeripheral;
import com.verr1.controlcraft.content.compact.createbigcannons.APAutocannonAccess;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.managers.ClientOutliner;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.ai.compact.links.CruiserPlant;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.BehaviorTree;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.*;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.*;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.EvadeEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.EvadeExitCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.PullEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.PullExitCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirAwareness;
import com.verr1.controlcraft.unstable.ai.game.cruiser.v1.DubinsHolderV2;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.ConstantCruiseNavigator;
import com.verr1.controlcraft.unstable.valkyrienskies.context.*;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.awt.*;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class CruiserBlockEntity extends OnShipBlockEntity implements
        IPlant, IAirContext
{


    public static final Address<AirAwareness> AWARENESS = new Address<>("awareness", AirAwareness.class);


    private final Blackboard storage = new Blackboard();
    private final BehaviorTree ai;
    private final CruiserControllerV4 cruiseController = new CruiserControllerV4();


    private final CruiserPlant plant = new CruiserPlant(this);
    private final AirAwareness awareness = new AirAwareness(this);


    private final PoseController poseController = new PoseController();


    private String debugTargetName = "@Target";

    private double cruiseVelocity = 50;
    private double shootTolerance = 7.5;

    private boolean useActualFlight = false;
    private boolean useActualWeapon = false;


    public CruiserBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);



        buildRegistry(SharedAIKeys.VEL)
                .withBasic(SerializePort.of(this::cruiseVelocity, this::setCruiseVelocity, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(SharedAIKeys.RAD)
                .withBasic(SerializePort.of(this::cruiseRadius, this::setCruiseRadius, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(SharedAIKeys.TOL)
                .withBasic(SerializePort.of(this::shootTolerance, this::setShootTolerance, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(SharedAIKeys.TAR)
                .withBasic(SerializePort.of(this::debugTargetName, this::setDebugTargetName, SerializeUtils.STRING))
                .withClient(ClientBuffer.STRING.get())
                .register();

        buildRegistry(SharedAIKeys.TWI)
                .withBasic(SerializePort.of(this::twistOmega, this::setTwistOmega, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(SharedAIKeys.YAW)
                .withBasic(SerializePort.of(this::yawOmega, this::setYawOmega, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(SharedAIKeys.ACTUAL_FLIGHT)
                .withBasic(SerializePort.of(this::useActualFlight, this::setUseActualFlight, SerializeUtils.BOOLEAN))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();

        buildRegistry(SharedAIKeys.ACTUAL_WEAPON)
                .withBasic(SerializePort.of(this::useActualWeapon, this::setUseActualWeapon, SerializeUtils.BOOLEAN))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();

        storage.set(SharedAIKeys.CONTEXT, this);
        storage.set(AWARENESS, awareness);

        Node root = new Parallel(ParallelPolicy.SUCCEED_ON_ALL).addChild(
                new Always(
                        new ThenUntilElse(
                                new PullEnterCondition(),
                                new PivotUpAction(),
                                new PullExitCondition(),
                                new ThenUntilElse(
                                        new EvadeEnterCondition(),
                                        new PivotAwayAction(),
                                        new EvadeExitCondition(),
                                        new PivotToAction()
                                )
                        )
                    ),
                new FireAction(),
                new AwarenessAction()
        );


        ai = new BehaviorTree(root);

    }

    public AirAwareness awareness() {
        return awareness;
    }

    public boolean useActualWeapon() {
        return useActualWeapon;
    }

    public void setUseActualWeapon(boolean useActualWeapon) {
        this.useActualWeapon = useActualWeapon;
    }

    public boolean useActualFlight() {
        return controller().useActualFlight();
    }

    public void setUseActualFlight(boolean useActualFlight) {
        controller().setUseActualFlight(useActualFlight);
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
    }

    @Override
    public Level world() {
        return getLevel();
    }

    @Override
    public Long shipId() {
        return getShipOrGroundID();
    }

    public double cruiseRadius(){
        return controller().radius();
    }

    public void setCruiseRadius(double radius){
        controller().setRadius(radius);
    }



    public LogicalDirectionTarget getLogical(){
        return new LogicalDirectionTarget(cruiseController, poseController, useActualFlight());
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
        if(useActualWeapon())return;

        Objects.requireNonNull(level);
        Vector3dc p = readSelf().position();
        Vector3dc front = readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1));
        Vector3dc spawn = p.fma(10.0, front, new Vector3d());
        APAutocannonAccess ap = CreateBigCannonsCompact.createAutocannonAp(level);
        if(ap == null)return;
        ap.setPos(toMinecraft(spawn));
        ap.setTracer(true);
        ap.setLifetime(40);
        ap.shoot(direction.x(), direction.y(), direction.z(), 9, 0);
        ap.addToLevel();
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
        return handler().readClientBuffer(SharedAIKeys.GOAL, Vector3dc.class);
    }

    private DubinsHolderV2 readClientPath(){
        return handler().readClientBuffer(SharedAIKeys.PATH, DubinsHolderV2.class);
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
        syncForAllPlayers(false, SharedAIKeys.GOAL, SharedAIKeys.PATH);
        syncCruiseTarget();
    }

    @Override
    public @NotNull NamedComponent plant() {
        return plant;
    }
}
