package com.verr1.controlcraft.unstable.blocks.monitor;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.content.blocks.spinalyzer.SpinalyzerBlockEntity;
import com.verr1.controlcraft.content.cctweaked.peripheral.SpinalyzerPeripheral;
import com.verr1.controlcraft.content.compact.createbigcannons.APAutocannonAccess;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.content.valkyrienskies.attachments.Observer;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.data.ExpirableListener;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.unstable.ai.api.IFighterJetContext;
import com.verr1.controlcraft.unstable.ai.compact.links.MonitorPlant;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.BehaviorTree;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.*;
import com.verr1.controlcraft.unstable.ai.game.PivotAwayAction;
import com.verr1.controlcraft.unstable.ai.game.PivotToAction;
import com.verr1.controlcraft.unstable.ai.game.PivotUpAction;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.*;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.EvadeEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.EvadeExitCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.FighterPullEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.FighterPullExitCondition;
import com.verr1.controlcraft.unstable.valkyrienskies.context.CruiseMonitor;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Objects;
import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class MonitorBlockEntity extends OnShipBlockEntity implements
        IFighterJetContext, IPlant
{
    public static NetworkKey P_COMMON = NetworkKey.create("p_common");
    public static NetworkKey P_PITCH = NetworkKey.create("p_pitch");
    public static NetworkKey P_YAW = NetworkKey.create("p_yaw");
    public static NetworkKey P_AG_ROLL = NetworkKey.create("p_ag_roll");
    public static NetworkKey P_LV_ROLL = NetworkKey.create("p_lv_roll");

    public static final Address<AirBaseAwareness> AWARENESS = new Address<>("awareness", AirBaseAwareness.class);


    private final Blackboard storage = new Blackboard();
    private final BehaviorTree ai;
    private final CruiseMonitor cruiseController = new CruiseMonitor();


    private final MonitorPlant plant = new MonitorPlant(this);
    private final AirBaseAwareness awareness = new AirBaseAwareness(this);

    private String debugTargetName = "@Target";

    private double cruiseVelocity = 50;
    private double cruiseRadius = 20;
    private double shootTolerance = 7.5;

    private boolean useActualWeapon = false;


    public MonitorBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);



        buildRegistry(SharedAIKeys.VEL)
                .withBasic(SerializePort.of(this::cruiseVelocity, this::setCruiseVelocity, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(SharedAIKeys.RAD)
                .withBasic(SerializePort.of(this::extremeRadius, this::setCruiseRadius, SerializeUtils.DOUBLE))
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

        buildRegistry(SharedAIKeys.ACTUAL_WEAPON)
                .withBasic(SerializePort.of(this::useActualWeapon, this::setUseActualWeapon, SerializeUtils.BOOLEAN))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();

        buildRegistry(P_COMMON)
                .withBasic(SerializePort.of(() -> controller().p(), p -> controller().setP(p), SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(P_PITCH)
                .withBasic(SerializePort.of(() -> controller().pPitch(), p -> controller().setPPitch(p), SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(P_YAW)
                .withBasic(SerializePort.of(() -> controller().pYaw(), p -> controller().setPYaw(p), SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(P_AG_ROLL)
                .withBasic(SerializePort.of(() -> controller().pAgRoll(), p -> controller().setPAgRoll(p), SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(P_LV_ROLL)
                .withBasic(SerializePort.of(() -> controller().pLvRoll(), p -> controller().setPLvRoll(p), SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();


        storage.set(SharedAIKeys.FIGHTER_CONTEXT, this);
        storage.set(AWARENESS, awareness);

        Node root = new Parallel(ParallelPolicy.SUCCEED_ON_ALL).addChild(
                new Always(
                        new ThenUntilElse(
                                new FighterPullEnterCondition(),
                                new PivotUpAction(),
                                new FighterPullExitCondition(),
                                new ThenUntilElse(
                                        new EvadeEnterCondition(),
                                        new PivotAwayAction(),
                                        new EvadeExitCondition(),
                                        new PivotToAction()
                                )
                        )
                ),
                new FireAction(),
                new FighterAwarenessAction()
        );


        ai = new BehaviorTree(root);

    }

    public AirBaseAwareness awareness() {
        return awareness;
    }

    public boolean useActualWeapon() {
        return useActualWeapon;
    }

    public void setUseActualWeapon(boolean useActualWeapon) {
        this.useActualWeapon = useActualWeapon;
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

    @Override
    public double extremeRadius(){
        return cruiseRadius;
    }

    public void setCruiseRadius(double radius){
        cruiseRadius = radius;
    }


    public @NotNull CruiseMonitor controller(){
        return cruiseController;
    }

    @Override
    public Vector3dc getTargetPosition() {
        return debug_getTarget();
    }

    @Override
    public Vector3dc getTargetVelocity() {
        return debug_getTargetVelocity();
    }

    @Override
    public double cruiseRadius() {
        return 60;
    }

    public void syncPose(){
        Optional
                .ofNullable(getLoadedServerShip())
                .map(Observer::getOrCreate)
                .ifPresent(ob -> ob.replace(
                        WorldBlockPos.of(level, getBlockPos()),
                        new ExpirableListener<>(
                                sp -> {
                                    controller().overridePose(sp.quaternion());
                                    controller().nextView();
                                },
                                10
                        )
                ));
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

    @Override
    public Quaterniondc getRotation() {
        return readSelf().quaternion();
    }

    public Vector3dc getHeading(){
        return readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1)); // assuming is facing +z
    }

    @Override
    public void kill() {

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



    @Override
    public void tickClient() {
        super.tickClient();
    }

    @Override
    public void tickServer() {
        super.tickServer();
        tickAI();
        syncPose();
    }


    public @NotNull NamedComponent plant() {
        return plant;
    }
}
