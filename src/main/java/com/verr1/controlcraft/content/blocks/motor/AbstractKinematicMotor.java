package com.verr1.controlcraft.content.blocks.motor;

import com.simibubi.create.foundation.utility.Couple;
import com.verr1.controlcraft.content.create.KMotorKineticPeripheral;
import com.verr1.controlcraft.content.valkyrienskies.controls.InducerControls;
import com.verr1.controlcraft.content.valkyrienskies.transform.LerpedTransformProvider;
import com.verr1.controlcraft.content.gui.layouts.api.IKinematicUIDevice;
import com.verr1.controlcraft.foundation.api.IKineticPeripheral;
import com.verr1.controlcraft.foundation.api.delegate.IKineticDevice;
import com.verr1.controlcraft.foundation.data.GroundBodyShip;
import com.verr1.controlcraft.foundation.data.NumericField;
import com.verr1.controlcraft.foundation.data.control.Pose;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.content.cctweaked.peripheral.KinematicMotorPeripheral;
import com.verr1.controlcraft.foundation.api.IPacketHandler;
import com.verr1.controlcraft.foundation.data.control.KinematicController;
import com.verr1.controlcraft.foundation.data.logical.LogicalKinematicMotor;
import com.verr1.controlcraft.foundation.redstone.DirectReceiver;
import com.verr1.controlcraft.foundation.redstone.IReceiver;
import com.verr1.controlcraft.foundation.type.descriptive.SlotType;
import com.verr1.controlcraft.foundation.type.descriptive.TargetMode;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.VSMathUtils;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.shared.Capabilities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.*;
import org.valkyrienskies.core.api.ships.LoadedServerShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.apigame.joints.VSFixedJoint;
import org.valkyrienskies.core.apigame.joints.VSJoint;
import org.valkyrienskies.core.apigame.joints.VSJointMaxForceTorque;
import org.valkyrienskies.core.apigame.joints.VSJointPose;

import java.lang.Math;
import java.util.Optional;

import static com.verr1.controlcraft.content.blocks.SharedKeys.*;

public abstract class AbstractKinematicMotor extends AbstractMotor implements
        IReceiver, IPacketHandler, IKinematicUIDevice, IKineticDevice
{
    protected KinematicController controller = new KinematicController();

    protected double compliance = -5;

    protected TargetMode mode = TargetMode.VELOCITY;

    protected boolean USE_CONSTRAINT_SPAMMING = true;

    protected double targetOfLastAppliedConstraint = 114514; // magic number : )

    private final DirectReceiver receiver = new DirectReceiver();

    private KinematicMotorPeripheral peripheral;
    private LazyOptional<IPeripheral> peripheralCap;
    private final KMotorKineticPeripheral kineticPeripheral = new KMotorKineticPeripheral(this);

    @Override
    public IKineticPeripheral peripheral() {
        return kineticPeripheral;
    }

    @Override
    public DirectReceiver receiver() {
        return receiver;
    }

    @Override
    public @NotNull <T> LazyOptional<T> getCapability(@NotNull Capability<T> cap, @org.jetbrains.annotations.Nullable Direction side) {
        if(cap == Capabilities.CAPABILITY_PERIPHERAL){
            if(this.peripheral == null){
                this.peripheral = new KinematicMotorPeripheral(this);
            }
            if(peripheralCap == null || !peripheralCap.isPresent())
                peripheralCap =  LazyOptional.of(() -> this.peripheral);
            return peripheralCap.cast();
        }
        return super.getCapability(cap, side);
    }


    public void setCompliance(double compliance) {
        this.compliance = compliance;
        setChanged();
    }

    public double getCompliance() {
        return compliance;
    }

    public TargetMode getTargetMode() {
        return mode;
    }

    public void setTargetMode(TargetMode mode) {
        this.mode = mode;
        setChanged();
    }

    public KinematicController getController() {
        return controller;
    }


    @Override
    public String name() {
        return "constraint_motor";
    }

    public void setMode(boolean isAdjustingAngle) {
        this.mode = isAdjustingAngle ? TargetMode.POSITION : TargetMode.VELOCITY;
        setChanged();
    }

    public AbstractKinematicMotor(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        registerConstraintKey("control");


        buildRegistry(COMPLIANCE).withBasic(SerializePort.of(this::getCompliance, this::setCompliance, SerializeUtils.DOUBLE)).withClient(ClientBuffer.DOUBLE.get()).register();
        buildRegistry(TARGET_MODE)
                .withBasic(SerializePort.of(this::getTargetMode, this::setTargetMode, SerializeUtils.ofEnum(TargetMode.class)))
                .withClient(ClientBuffer.of(TargetMode.class))
                .register();
        buildRegistry(CONNECT_CONTEXT).withBasic(SerializePort.of(() -> context, ctx -> context = ctx, SerializeUtils.CONNECT_CONTEXT)).register();

        buildRegistry(TARGET).withBasic(SerializePort.of(() -> getController().getControlTarget(), t -> getController().setControlTarget(t), SerializeUtils.DOUBLE)).withClient(ClientBuffer.DOUBLE.get()).register();
        buildRegistry(VALUE).withBasic(SerializePort.of(() -> getController().getTarget(), $ -> {}, SerializeUtils.DOUBLE)).withClient(ClientBuffer.DOUBLE.get()).register();
        buildRegistry(PLACE_HOLDER)
                .withBasic(CompoundTagPort.of(
                        CompoundTag::new,
                        $ ->  {if(getTargetMode() == TargetMode.VELOCITY)getController().setTarget(0);}
                ))
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

        receiver().register(
                new NumericField(
                        () -> getController().getTarget(),
                        t -> getController().setControlTarget(t),
                        "target"
                ),
                new DirectReceiver.InitContext(SlotType.TARGET, Couple.create(-Math.PI, Math.PI)),
                6
        );
    }


    private void tickTarget(){
        if(mode == TargetMode.VELOCITY){
            controller.updateTargetAngular(0.05);
        }else{
            controller.updateForcedTarget();
        }
    }

    private void tickConstraint(){
        tickTarget();
        if(Math.abs(targetOfLastAppliedConstraint - controller.getTarget()) < Math.pow(10, compliance) + 1e-6)return;
        if(level == null || level.isClientSide)return;
        long compID = Optional.ofNullable(getCompanionServerShip()).map(Ship::getId).orElse(-1L);
        if(compID == -1)return;
        Quaterniondc q_self = new
                Quaterniond(VSMathUtils.getQuaternionOfPlacement(getServoDirection()))
                .mul(new Quaterniond(new AxisAngle4d(Math.toRadians(90.0), 0.0, 0.0, 1.0)), new Quaterniond())
                .normalize();


        double AngleFix = VSMathUtils.getDumbFixOfLockMode(getServoDirection(), getCompanionShipAlign());

        Quaterniondc q_comp = new Quaterniond()
                .rotateAxis(AngleFix - getController().getTarget(), getCompanionShipAlignJOML())  // dumbFix +  dumb fixing getServoDirectionJOML()
                .mul(VSMathUtils.getQuaternionOfPlacement(getCompanionShipAlign().getOpposite()))
                .mul(new Quaterniond(new AxisAngle4d(Math.toRadians(90.0), 0.0, 0.0, 1.0)), new Quaterniond())
                .normalize();

        Vector3dc v_own = q_self.transform(new Vector3d(0, 1, 0));
        Vector3dc v_cmp = q_comp.transform(new Vector3d(0, 1, 0));


        VSJoint joint = new VSFixedJoint(
                getShipOrGroundID(),
                new VSJointPose(context.self().getPos(), q_self),
                compID,
                new VSJointPose(context.comp().getPos(), q_comp),
                new VSJointMaxForceTorque(1e20f, 1e20f)
        );
        overrideConstraint("control", joint);
        targetOfLastAppliedConstraint = controller.getTarget();
    }

    @Override
    public void destroyConstraints() {
        Optional.ofNullable(getCompanionServerShip()).ifPresent(s -> s.setStatic(false));
        clearCompanionShipInfo();
        super.destroyConstraints(); // set non-static before ship info is cleared
        if(USE_CONSTRAINT_SPAMMING){
            destroyConstraintForMode();
        }
    }

    private void destroyConstraintForMode(){
        removeConstraint("revolute");
        removeConstraint("attach_1");
        removeConstraint("attach_2");
        removeConstraint("control");
    }

    @Override
    public void bruteDirectionalConnectWith(BlockPos bp_comp, Direction dir_comp, Direction forward) {
        super.bruteDirectionalConnectWith(bp_comp, dir_comp, forward);
        if(!USE_CONSTRAINT_SPAMMING){
            destroyConstraintForMode();
        }
    }

    @Override
    public void setStartingAngleOfCompanionShip() {
        Ship asm = getCompanionServerShip();
        Ship own = getShipOn();
        if(asm == null)return;
        double target = VSMathUtils.get_yc2xc(own, asm, getServoDirection(), getCompanionShipAlign());
        controller.setTarget(target);
    }

    @Override
    public void assemble() {
        super.assemble();
        if(!USE_CONSTRAINT_SPAMMING){
            destroyConstraintForMode();
        }
    }

    public @Nullable Pose tickPose(){
        LoadedServerShip compShip = getCompanionServerShip();
        LogicalKinematicMotor motor = getLogicalMotor();
        Ship selfShip = getShipOn();
        if(compShip == null || motor == null)return null;
        return InducerControls.kinematicMotorTickControls(
                motor,
                Optional.ofNullable(selfShip).orElse(new GroundBodyShip()),
                compShip
        );
    }

    @Override
    public void tickServer() {
        super.tickServer();
        syncForNear(true, FIELD);
        tickConstraint();
        kineticPeripheral.tick();
    }

    public void syncAttachInducer(){
        if(level == null || level.isClientSide)return;
        /*
        Optional
                .ofNullable(getCompanionServerShip())
                .map(KinematicMotorForceInducer_::getOrCreate)
                .ifPresent(inducer -> inducer.alive(WorldBlockPos.of(level, getBlockPos())));
        * */

    }

    // simply for debugging


    @Override
    public void tickClient() {
        super.tickClient();
        // syncAttachTransformProviderClient();
    }



    /*
    *
    public void syncAttachTransformProviderClient(){
        if(level != null && !level.isClientSide)return;
        Optional
                .ofNullable(getCompanionClientShip())
                .ifPresent(LerpedTransformProvider::replaceOrCreate);
    }
    public void syncAttachTransformProviderServer(){
        if(level != null && level.isClientSide)return;
        Optional
                .ofNullable(getCompanionServerShip())
                .map(KinematicMotorTransformProvider::replaceOrCreate)
                .ifPresent(prov -> Optional.ofNullable(tickPose()).ifPresent(prov::set));
    }
    * */


    public @Nullable LogicalKinematicMotor getLogicalMotor() {
        if(level == null || level.isClientSide)return null;
        if(noCompanionShip() || context.isDirty())return null;
        return new LogicalKinematicMotor(
                getShipOrGroundID(),
                getCompanionShipID(),
                context,
                getTargetMode() == TargetMode.POSITION,
                getServoDirection(),
                getCompanionShipAlign(),
                controller
        );
    }
}
