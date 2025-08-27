package com.verr1.controlcraft.unstable.blocks;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.content.blocks.spinalyzer.SpinalyzerBlockEntity;
import com.verr1.controlcraft.content.cctweaked.peripheral.SpinalyzerPeripheral;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.ai.core.BehaviorTree;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;
import com.verr1.controlcraft.unstable.data.schematic.AISchematic;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.AIBlockNetwork;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.ConstantCruiseNavigator;
import com.verr1.controlcraft.unstable.valkyrienskies.context.CruiseController;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalDirectionTarget;
import com.verr1.controlcraft.unstable.valkyrienskies.context.PoseController;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.ServerShip;
import rbasamoyai.createbigcannons.CreateBigCannons;
import rbasamoyai.createbigcannons.munitions.ImpactExplosion;
import rbasamoyai.createbigcannons.munitions.ShellExplosion;
import rbasamoyai.createbigcannons.munitions.autocannon.flak.FlakExplosion;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

import static com.verr1.controlcraft.unstable.blocks.monitor.MonitorBlockEntity.*;

public abstract class AiPlaneBase extends AIBaseBlockEntity implements
        IAirContext
{


    protected final Blackboard storage = new Blackboard();
    protected final BehaviorTree ai;
    protected final CruiseController cruiseController = new CruiseController();
    protected final PoseController poseController = new PoseController();

    protected final AirBaseAwareness awareness = new AirBaseAwareness(this);

    protected String debugTargetName = "";
    protected double cruiseVelocity = 50;

    private boolean isDead = false;

    public AiPlaneBase(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        registerDouble(this::cruiseVelocity, this::setCruiseVelocity, SharedAIKeys.VEL);
        registerDouble(this::extremeRadius, this::setExtremeRadius, SharedAIKeys.E_RAD);
        registerDouble(this::twistOmega, this::setTwistOmega, SharedAIKeys.TWI);
        registerDouble(this::yawOmega, this::setYawOmega, SharedAIKeys.YAW);


        registerDouble(this::pDrive, this::setPDrive, SharedAIKeys.P_DRIVE);
        registerDouble(this::iDrive, this::setIDrive, SharedAIKeys.I_DRIVE);
        registerDouble(this::turnResistance, this::setTurnResistance, SharedAIKeys.TURN_RESIST);
        registerDouble(() -> controller().pCom(), p -> controller().setPCom(p), P_COMMON);
        registerDouble(() -> controller().pPitch(), p -> controller().setPPitch(p), P_PITCH);
        registerDouble(() -> controller().pYaw(), p -> controller().setPYaw(p), P_YAW);
        registerDouble(() -> controller().pAgRoll(), p -> controller().setPAgRoll(p), P_AG_ROLL);
        registerDouble(() -> controller().pLvRoll(), p -> controller().setPLvRoll(p), P_LV_ROLL);

        buildRegistry(SharedAIKeys.TAR)
                .withBasic(SerializePort.of(this::debugTargetName, this::setDebugTargetName, SerializeUtils.STRING))
                .withClient(ClientBuffer.STRING.get())
                .register();

        ai = constructAI();
    }

    protected void registerDouble(Supplier<Double> getter, Consumer<Double> setter, NetworkKey key){
        register(getter, setter, SerializeUtils.DOUBLE, ClientBuffer.DOUBLE.get(), key);
    }

    protected void registerBoolean(Supplier<Boolean> getter, Consumer<Boolean> setter, NetworkKey key){
        register(getter, setter, SerializeUtils.BOOLEAN, ClientBuffer.BOOLEAN.get(), key);
    }

    protected<T> void register(Supplier<T> getter, Consumer<T> setter, Serializer<T> ser, ClientBuffer<T> buf, NetworkKey key){
        buildRegistry(key)
                .withBasic(SerializePort.of(getter, setter, ser))
                .withClient(buf)
                .register();
    }

    protected abstract BehaviorTree constructAI();

    public AirBaseAwareness awareness() {
        return awareness;
    }

    public double pDrive(){
        return controller().pDrive();
    }

    public double iDrive(){
        return controller().iDrive();
    }

    public void setPDrive(double p_drive){
        controller().setPDrive(p_drive);
    }

    public void setIDrive(double i_drive){
        controller().setIDrive(i_drive);
    }

    public double turnResistance() {
        return controller().turnResistance();
    }

    public void setTurnResistance(double turnResistance) {
        controller().setTurnResistance(turnResistance);
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

    public double cruiseVelocity() {
        return controller().velocity();
    }

    public void setCruiseVelocity(double cruiseVelocity) {
        controller().setVelocity(cruiseVelocity);
    }

    @Override
    public CruiseController controller() {
        return cruiseController;
    }

    @Override
    public Level world() {
        return getLevel();
    }

    @Override
    public Long shipId() {
        return getShipOrGroundID();
    }

    public double extremeRadius(){
        return controller().radius();
    }

    public void setExtremeRadius(double radius){
        controller().setRadius(radius);
    }


    public boolean isDead() {
        return isDead;
    }

    public void tickHealth(){
        long id = getShipOrGroundID();
        ServerShip ship = getLoadedServerShip();
        if(ship == null)return;
        if(!AIServer.MANAGER.isAI(id))return;
        double mass = AIServer.MANAGER
                .getDataOf(id)
                .map(d -> d.key)
                .map(AIServer.SCHEMATICS_MANAGER::getLoaded)
                .map(AISchematic::mass)
                .orElse(-1.0);
        if(mass < 0)return;
        double currentMass = ship.getInertiaData().getMass();

        if(currentMass / mass > 0.9)return;
        scheduleDeath();
    }

    public void scheduleDeath(){
        if(!isAI() || isDead)return;
        isDead = true;
        ControlCraftServer.SERVER_EXECUTOR.executeOnSchedule(getBlockPos().toShortString() + " + explode", this::explode, 10, 3);
        ControlCraftServer.SERVER_EXECUTOR.executeLater(getBlockPos().toShortString() + " + discard", this::discard, 50);
    }


//    @Override
//    public void discard() {
//        super.discard();
//        isDead = false;
//        awareness.resetStuckScore();
//        awareness.resetAttackScore();
//    }

    @Override
    public void onSpawn() {
        isDead = false;
        awareness.resetStuckScore();
        poseController.overrideTarget(getRotation());
    }

    public void syncNetwork(){
        Optional.ofNullable(getLoadedServerShip())
                .map(AIBlockNetwork::getOrCreate)
                .ifPresent(s -> s.activateListener(getWorldBlockPos(), this));
    }

    public void explode(){
        if(level == null)return;
        Vector3dc p = getPosition();
        ImpactExplosion impact = new ImpactExplosion(
                level,
                null,
                null,
                p.x(), p.y(), p.z(),
                3,
                Level.ExplosionInteraction.NONE
        );
        ShellExplosion impact2 = new ShellExplosion(
                level,
                null,
                null,
                p.x(), p.y(), p.z(),
                3,
                false,
                Level.ExplosionInteraction.NONE
        );
        FlakExplosion impact3 = new FlakExplosion(
                level,
                null,
                null,
                p.x(), p.y(), p.z(),
                3,
                Level.ExplosionInteraction.NONE
        );
        CreateBigCannons.handleCustomExplosion(level, impact);
        CreateBigCannons.handleCustomExplosion(level, impact2);
        CreateBigCannons.handleCustomExplosion(level, impact3);
        // level.explode(null, p.x(), p.y(), p.z(), 4, Level.ExplosionInteraction.NONE);
    }

    public LogicalDirectionTarget getLogical(){
        return new LogicalDirectionTarget(controller(), poseController(), pDrive());
    }

    public PoseController poseController() {
        return poseController;
    }

    @Override
    public void tickServer() {
        super.tickServer();
    }

    @Override
    public void kill() {
        if(!isAI())return;
        scheduleDeath();
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

    protected void tickAI(){
        ServerShip ship = getLoadedServerShip();
        if(ship == null || ship.isStatic() || isDead())return;
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

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        tickHealth();
        syncNetwork();
    }
}
