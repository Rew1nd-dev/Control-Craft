package com.verr1.controlcraft.unstable.blocks.attacker;

import com.cainiao1053.cbcmoreshells.CBCMSEntityTypes;
import com.cainiao1053.cbcmoreshells.munitions.big_cannon.aphe_cannon_rocket.APHECannonRocketProjectile;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.content.blocks.spinalyzer.SpinalyzerBlockEntity;
import com.verr1.controlcraft.content.cctweaked.peripheral.SpinalyzerPeripheral;
import com.verr1.controlcraft.content.compact.createbigcannons.APAutocannonAccess;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.managers.ClientOutliner;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.api.IFighterJetContext;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.BehaviorTree;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.*;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.attacker.*;
import com.verr1.controlcraft.unstable.ai.game.attacker.conditions.AttackEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.attacker.conditions.FighterEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.attacker.conditions.PullEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.attacker.conditions.PullExitCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AwarenessAction;
import com.verr1.controlcraft.unstable.blocks.AiPlaneBase;
import com.verr1.controlcraft.unstable.pathing.path.IPath;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.ConstantCruiseNavigator;
import com.verr1.controlcraft.unstable.valkyrienskies.context.CruiseController;
import com.verr1.controlcraft.unstable.valkyrienskies.context.LogicalDirectionTarget;
import com.verr1.controlcraft.unstable.valkyrienskies.context.PoseController;
import com.verr1.controlcraft.utils.MathUtils;
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
import org.valkyrienskies.core.api.ships.ServerShip;

import java.awt.*;
import java.util.Objects;
import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;
import static com.verr1.controlcraft.unstable.ai.game.SharedAIKeys.*;

public class AiAttackerBlockEntity extends AiPlaneBase implements
    IAttackerContext
{



    public static Address<IAttackerContext> CONTEXT = new Address<>("cruiser", IAttackerContext.class);
    public static Address<IPath> CURRENT_CRUISE = new Address<>("cruiser_current_cruise", IPath.class);


    private double cruiseRadius = 20;
    private double cruiseVelocity = 50;
    private double shootTolerance = 7.5;


    public AiAttackerBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);


        registerDouble(this::shootTolerance, this::setShootTolerance, TOL);
        registerDouble(this::extremeRadius, this::setExtremeRadius, RAD);

        storage.set(CONTEXT, this);
        storage.set(AWARENESS, awareness);


    }

    @Override
    protected BehaviorTree constructAI() {
        Node root = new Parallel(ParallelPolicy.SUCCEED_ON_ALL).addChild(
                new Always(
                        new ThenUntilElse(
                                new PullEnterCondition(),
                                new AvoidCollideAction(),
                                new PullExitCondition(),
                                new ThenUntilElse(
                                        new AttackEnterCondition(),
                                        new Sequence().addChild(
                                                new MakePathAction(),
                                                new PathAlongAction(),
                                                new TossAction()
                                        ),
                                        new Inverter(new AttackEnterCondition()),
                                        new ThenUntilElse(
                                                new FighterEnterCondition(),
                                                null,
                                                new Inverter(new FighterEnterCondition()),
                                                new CirclingAction()
                                        )
                                )
                        )
                ),
                new AnchorAction()
        );
        return new BehaviorTree(root);
    }


    public double shootTolerance() {
        return shootTolerance;
    }

    @Override
    public void fireAt(Vector3dc direction) {
        if(isClientSide())return;
        Objects.requireNonNull(level);
        Vector3dc p = readSelf().position();
        Vector3dc front = readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1));
        Vector3dc spawn = p.fma(10.0, front, new Vector3d());
        APHECannonRocketProjectile ap = new APHECannonRocketProjectile(CBCMSEntityTypes.APHE_CANNON_ROCKET.get(), level);

        ap.setPos(toMinecraft(spawn));
        ap.setLifetime(40);
        ap.shoot(direction.x(), direction.y(), direction.z(), 9, 0);
        level.addFreshEntity(ap);
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

    @Override
    public Level world() {
        return getLevel();
    }

    @Override
    public Long shipId() {
        return getShipOrGroundID();
    }

    public double cruiseRadius(){
        return cruiseRadius;
    }

    @Override
    public double extremeRadius() {
        return controller().radius();
    }

    public void setCruiseRadius(double radius){
        cruiseRadius = radius;
    }

    public void setExtremeRadius(double radius){
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



    public @NotNull CruiseController controller(){
        return cruiseController;
    }

    @Override
    public @NotNull Vector3dc getGroundTarget() {
        return Optional.ofNullable(debug_getTarget()).orElse(below());
    }

    @Override
    public @NotNull Vector3dc getGroundTargetVelocity() {
        return Optional.ofNullable(debug_getTargetVelocity()).orElse(new Vector3d());
    }

    @Override
    public Vector3dc getAirTarget() {
        return null;
    }

    @Override
    public Vector3dc getAirTargetVelocity() {
        return null;
    }

    public Vector3dc below(){
        Vector3dc p = getPosition();
        double y = height();
        return new Vector3d(p.x(), y, p.z());
    }

    public void tickAwareness(){
        awareness.overrideDual(
                getGroundTarget(),
                getGroundTargetVelocity(),
                getPosition(),
                getVelocity(),
                getCruiseTarget(),
                getHeading()
        );
        awareness.tick();
    }

    public Vector3dc getCruiseTarget(){
        return getPosition().add(controller().targetDirection(), new Vector3d());
    }

    public @NotNull Vector3dc getPosition(){
        return readSelf().position();
    }

    public @NotNull Vector3dc getVelocity(){
        return readSelf().velocity();
    }

    public @NotNull Vector3dc getHeading(){
        return readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1)); // assuming is facing +z
    }

    @Override
    public Quaterniondc getRotation() {
        return readSelf().quaternion();
    }

    @Override
    public boolean noGroundTarget() {
        return debug_getTarget() == null;
    }

    @Override
    public boolean hasAirThreat() {
        return false;
    }


    @Override
    public void tickServer() {
        super.tickServer();
        tickAI();
        syncCruiseTarget();
        tickAwareness();
    }
}
