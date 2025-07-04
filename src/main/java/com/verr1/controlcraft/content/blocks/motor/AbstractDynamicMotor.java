package com.verr1.controlcraft.content.blocks.motor;

import com.simibubi.create.content.equipment.goggles.IHaveGoggleInformation;
import com.simibubi.create.foundation.utility.Couple;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.create.DMotorKineticPeripheral;
import com.verr1.controlcraft.content.valkyrienskies.attachments.DynamicMotorForceInducer;
import com.verr1.controlcraft.foundation.api.delegate.IKineticDevice;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.MotorPlant;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.content.cctweaked.peripheral.DynamicMotorPeripheral;
import com.verr1.controlcraft.foundation.api.delegate.IControllerProvider;
import com.verr1.controlcraft.foundation.api.IPacketHandler;
import com.verr1.controlcraft.foundation.data.*;
import com.verr1.controlcraft.foundation.data.control.DynamicController;
import com.verr1.controlcraft.foundation.data.logical.LogicalDynamicMotor;
import com.verr1.controlcraft.foundation.redstone.DirectReceiver;
import com.verr1.controlcraft.foundation.redstone.IReceiver;
import com.verr1.controlcraft.foundation.type.descriptive.CheatMode;
import com.verr1.controlcraft.foundation.type.descriptive.SlotType;
import com.verr1.controlcraft.foundation.type.descriptive.LockMode;
import com.verr1.controlcraft.foundation.type.descriptive.TargetMode;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.VSMathUtils;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.shared.Capabilities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import org.jetbrains.annotations.NotNull;
import org.joml.*;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.apigame.constraints.VSAttachmentConstraint;

import javax.annotation.Nullable;
import java.lang.Math;
import java.util.List;
import java.util.Optional;

import static com.verr1.controlcraft.content.blocks.SharedKeys.*;

public abstract class AbstractDynamicMotor extends AbstractMotor implements
        IHaveGoggleInformation, IControllerProvider, IReceiver,
        IPacketHandler, IKineticDevice, IPlant
{
    public SynchronizedField<Double> controlTorque = new SynchronizedField<>(0.0);
    private final DynamicController controller = new DynamicController().withPID(DEFAULT_VELOCITY_MODE_PARAMS);
    private boolean isLocked = false;
    private final DirectReceiver receiver = new DirectReceiver();
    private final MotorPlant plant = new MotorPlant(this);
    private double speedLimit = 10;
    protected TargetMode targetMode = TargetMode.VELOCITY;
    protected LockMode lockMode = LockMode.OFF;
    protected CheatMode cheatMode = CheatMode.NONE;
    protected boolean reverseCreateInput = false;
    private DynamicMotorPeripheral peripheral;
    private LazyOptional<IPeripheral> peripheralCap;
    private final DMotorKineticPeripheral kineticPeripheral = new DMotorKineticPeripheral(this);



    @Override
    public @NotNull MotorPlant plant() {
        return plant;
    }

    @Override
    public DirectReceiver receiver() {
        return receiver;
    }

    public void setTargetAccordingly(double target){
        switch (targetMode){
            case POSITION : controller.setTarget(MathUtils.clamp(target, Math.PI));
            case VELOCITY : controller.setTarget(target);
        }
        setChanged();
    }

    public void setLockMode(LockMode lockMode) {
        this.lockMode = lockMode;
        if(lockMode == LockMode.OFF)tryUnlock();
        setChanged();
    }

    public void setCheatMode(CheatMode cheatMode) {
        this.cheatMode = cheatMode;
        setChanged();
    }

    public void setReverseCreateInput(boolean reverseCreateInput) {
        this.reverseCreateInput = reverseCreateInput;
        setChanged();
    }

    public TargetMode getTargetMode() {return targetMode;}
    public LockMode getLockMode() {return lockMode;}
    public CheatMode getCheatMode() {return cheatMode;}
    public double getTarget(){return controller.getTarget();}
    public boolean isLocked() {return isLocked;}
    public double speedLimit() {return speedLimit;}
    public void setSpeedLimit(double speedLimit) {this.speedLimit = Math.max(speedLimit, 2);}
    public void setTargetMode(TargetMode targetMode) {
        this.targetMode = targetMode;
        /*
        // if(this.targetMode == targetMode)return;
        // delay this because client screen will also call to set PID values of last mode
        Runnable task = () -> {if(targetMode == TargetMode.POSITION){
            controller.PID(DEFAULT_POSITION_MODE_PARAMS);
        }
        if(targetMode == TargetMode.VELOCITY){
            controller.PID(DEFAULT_VELOCITY_MODE_PARAMS);
        }};

        if(level == null || level.isClientSide)return;
        ControlCraftServer.SERVER_EXECUTOR.executeLater(task, 1);
        * */
        setChanged();
    }

    @Override
    public DMotorKineticPeripheral peripheral() {
        return kineticPeripheral;
    }

    @Override
    public @NotNull <T> LazyOptional<T> getCapability(@NotNull Capability<T> cap, @Nullable Direction side) {
        if(cap == Capabilities.CAPABILITY_PERIPHERAL){
            if(this.peripheral == null){
                this.peripheral = new DynamicMotorPeripheral(this);
            }
            if(peripheralCap == null || !peripheralCap.isPresent())
                peripheralCap =  LazyOptional.of(() -> this.peripheral);
            return peripheralCap.cast();
        }
        return super.getCapability(cap, side);
    }

    @Override
    public String name() {
        return "motor";
    }

    public void tryLock(){
        if(isLocked)return;
        isLocked = true;
        lock();
    }

    public void tryLock(boolean toLock){
        if (toLock){
            tryLock();
        }else {
            tryUnlock();
        }
    }

    private void lock(){
        if(level == null || level.isClientSide)return;
        if(noCompanionShip() || context.isDirty())return;

        Quaterniondc q_self = new
                Quaterniond(VSMathUtils.getQuaternionOfPlacement(getServoDirection()))
                .mul(new Quaterniond(new AxisAngle4d(Math.toRadians(90.0), 0.0, 0.0, 1.0)), new Quaterniond())
                .normalize();


        double AngleFix = VSMathUtils.getDumbFixOfLockMode(getServoDirection(), getCompanionShipAlign());

        Quaterniondc q_comp = new Quaterniond()
                .rotateAxis(AngleFix - getServoAngle(), getCompanionShipAlignJOML())  // dumbFix
                .mul(VSMathUtils.getQuaternionOfPlacement(getCompanionShipAlign().getOpposite()))
                .mul(new Quaterniond(new AxisAngle4d(Math.toRadians(90.0), 0.0, 0.0, 1.0)), new Quaterniond())
                .normalize();

        Vector3dc v_own = q_self.transform(new Vector3d(0, 1, 0));
        Vector3dc v_cmp = q_comp.transform(new Vector3d(0, 1, 0));

        /*
        * VSFixedJoint joint = new VSFixedJoint(
                getShipID(),
                new VSJointPose(context.self().getPos(), q_self),
                getCompanionShipID(),
                new VSJointPose(context.comp().getPos(), q_comp),
                new VSJointMaxForceTorque(1e20f, 1e20f)
        );
        * */

        VSAttachmentConstraint fixed = new VSAttachmentConstraint(
                getShipOrGroundID(),
                getCompanionShipID(),
                1.0E-20,
                context.self().getPos().add(v_own, new Vector3d()),
                context.comp().getPos().add(v_cmp, new Vector3d()),
                1.0E20,
                0.0
        );

        overrideConstraint("fix", fixed);
        isLocked = true;
        setChanged();
    }

    public void tryUnlock(){
        if(!isLocked)return;
        isLocked = false;
        unlock();
    }

    private void unlock(){
        if(level == null || level.isClientSide)return;
        removeConstraint("fix");
        isLocked = false;
        setChanged();
    }

    public @Nullable LogicalDynamicMotor getLogicalMotor(){
        if(noCompanionShip())return null;
        return new LogicalDynamicMotor(
                getShipOrGroundID(),
                getCompanionShipID(),
                WorldBlockPos.of(level, getBlockPos()),
                getServoDirection(),
                getCompanionShipAlign(),
                targetMode == TargetMode.POSITION,
                ! cheatMode.shouldNoRepulse(),
                cheatMode.shouldEliminateGravity(),
                !isLocked(),
                controlTorque.read(),
                speedLimit(),
                getController(),
                this::setCachedServoAngle,
                this::setCachedServoAngularVelocity
        );
    }

    public void syncAttachInducer(){
        if(level == null || level.isClientSide)return;
        Optional
            .ofNullable(getCompanionServerShip())
            .map(DynamicMotorForceInducer::getOrCreate)
            .ifPresent(inducer -> inducer.replace(
                    WorldBlockPos.of(level, getBlockPos()),
                    this::getLogicalMotor
            ));
    }

    @Override
    public void tickServer() {
        super.tickServer();
        syncAttachInducer();
        lockCheck();
        kineticPeripheral.tick();
        // ExposedFieldSyncClientPacket.syncClient(this, getBlockPos(), level);
    }


    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        syncForNear(true, FIELD, CONTROLLER);
    }

    public void lockCheck(){
        if(lockMode != LockMode.ON)return;
        if(targetMode == TargetMode.POSITION){
            double compliance = isLocked() ? 5e-3 : 1e-3;
            // avoid locking at the edge of compliance which may unlock accidentally
            // when the companion ship is ongoing a strike
            if(Math.abs(getServoAngle() - getTarget()) < compliance){
                tryLock();
            }else{
                tryUnlock();
            }
        }
        if(targetMode == TargetMode.VELOCITY){
            if(Math.abs(getTarget()) < 1e-3){
                tryLock();
            }else{
                tryUnlock();
            }
        }
    }

    @Override
    public DynamicController getController() {return controller;}



    public void setOutputTorque(double torque){
        controlTorque.write(torque);
    }

    public double getOutputTorque(){
        return controlTorque.read();
    }

    @Override
    public void destroyConstraints() {
        super.destroyConstraints();
        removeConstraint("fix");
        controlTorque.write(0.0);

    }

    public void setMode(boolean adjustAngle){
        targetMode = adjustAngle ? TargetMode.POSITION : TargetMode.VELOCITY;
        setChanged();
    }

    public void toggleMode(){
        setMode(!(targetMode == TargetMode.POSITION));
    }


    public AbstractDynamicMotor(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        registerConstraintKey("fix");

        buildRegistry(CHEAT_MODE)
                .withBasic(SerializePort.of(
                        this::getCheatMode,
                        this::setCheatMode,
                        SerializeUtils.ofEnum(CheatMode.class)))
                .withClient(ClientBuffer.of(CheatMode.class))
                .register();

        buildRegistry(TARGET_MODE)
                .withBasic(SerializePort.of(this::getTargetMode, this::setTargetMode, SerializeUtils.ofEnum(TargetMode.class)))
                .withClient(ClientBuffer.of(TargetMode.class))
                .register();

        buildRegistry(LOCK_MODE)
                .withBasic(SerializePort.of(this::getLockMode, this::setLockMode, SerializeUtils.ofEnum(LockMode.class)))
                .withClient(ClientBuffer.of(LockMode.class))
                .register();

        buildRegistry(IS_LOCKED)
                .withBasic(SerializePort.of(this::isLocked, bl -> isLocked = bl, SerializeUtils.BOOLEAN))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();

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

        buildRegistry(SPEED_LIMIT)
                .withBasic(SerializePort.of(this::speedLimit, this::setSpeedLimit, SerializeUtils.DOUBLE))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(CONTROLLER)
                .withBasic(CompoundTagPort.of(
                        () -> getController().serialize(),
                        tag -> getController().deserialize(tag)
                ))
                .withClient(
                        new ClientBuffer<>(SerializeUtils.UNIT, CompoundTag.class)
                )
                .register();

        buildRegistry(TARGET)
                .withBasic(SerializePort.of(
                        () -> getController().getTarget(),
                        t -> getController().setTarget(t),
                        SerializeUtils.DOUBLE
                ))
                .withClient(
                        new ClientBuffer<>(SerializeUtils.DOUBLE, Double.class)
                )
                .register();

        buildRegistry(VALUE)
                .withBasic(SerializePort.of(
                        () -> getController().getValue(),
                        $ -> {},
                        SerializeUtils.DOUBLE
                ))
                .withClient(
                        new ClientBuffer<>(SerializeUtils.DOUBLE, Double.class)
                )
                .runtimeOnly()
                .register();

        buildRegistry(PLACE_HOLDER)
                .withBasic(CompoundTagPort.of(
                        CompoundTag::new,
                        $ ->  {if(targetMode == TargetMode.VELOCITY)getController().setTarget(0);}
                ))
                .register();


        panel().registerUnit(SharedKeys.LOCK, this::tryLock);
        panel().registerUnit(SharedKeys.UNLOCK, this::tryUnlock);
        panel().registerUnit(SharedKeys.DISASSEMBLE, this::destroyConstraints);

        receiver()
            .register(
                    new NumericField(
                            this::getOutputTorque,
                            this::setOutputTorque,
                            "torque"
                    ),
                    new DirectReceiver.InitContext(SlotType.TORQUE, Couple.create(0.0, 1000.0)),
                    2
        )
            .register(
                    new NumericField(
                            this::getTarget,
                            this::setTargetAccordingly,
                            "target"
                    ),
                    new DirectReceiver.InitContext(SlotType.TARGET, Couple.create(0.0, Math.PI / 2.0)),
                    6
        )
            .register(
                    new NumericField(
                            () -> isLocked() ? 1.0 : 0.0,
                            t -> {
                                if (t > 0.001) tryLock();
                                else tryUnlock();
                            },
                            "locked"
                    ),
                    new DirectReceiver.InitContext(SlotType.IS_LOCKED, Couple.create(0.0, 1.0)),
                    2
        );

    }




    @Override
    public void setStartingAngleOfCompanionShip(){
        Ship asm = getCompanionServerShip();
        Ship own = getShipOn();
        if(asm == null)return;
        if(targetMode != TargetMode.POSITION)return;
        double startAngle = VSMathUtils.get_yc2xc(own, asm, getServoDirection(), getCompanionShipAlign());
        getController().setTarget(startAngle);
    }

    @OnlyIn(Dist.CLIENT)
    @Override
    public boolean addToGoggleTooltip(List<Component> tooltip, boolean isPlayerSneaking) {
        return receiver().makeToolTip(tooltip, isPlayerSneaking);
    }

}
