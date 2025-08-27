package com.verr1.controlcraft.unstable.blocks.attacker;

import com.cainiao1053.cbcmoreshells.CBCMSEntityTypes;
import com.cainiao1053.cbcmoreshells.munitions.big_cannon.aphe_cannon_rocket.APHECannonRocketProjectile;
import com.verr1.controlcraft.foundation.managers.ClientOutliner;
import com.verr1.controlcraft.unstable.ai.api.IAnchorContext;
import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.api.ICircleContext;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.BehaviorTree;
import com.verr1.controlcraft.unstable.ai.core.nodes.*;
import com.verr1.controlcraft.unstable.ai.game.PivotAwayAction;
import com.verr1.controlcraft.unstable.ai.game.PivotToAction;
import com.verr1.controlcraft.unstable.ai.game.PivotUpAction;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.attacker.*;
import com.verr1.controlcraft.unstable.ai.game.attacker.conditions.*;
import com.verr1.controlcraft.unstable.ai.game.cruiser.SuicideAction;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.FighterPullEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.FighterPullExitCondition;
import com.verr1.controlcraft.unstable.blocks.AiPlaneBase;
import com.verr1.controlcraft.unstable.pathing.path.IPath;
import com.verr1.controlcraft.unstable.targeting.AirAttackerTargetSelector;
import com.verr1.controlcraft.unstable.valkyrienskies.context.CruiseController;
import com.verr1.controlcraft.utils.MathUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.projectile.Arrow;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.awt.*;
import java.util.Objects;
import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;
import static com.verr1.controlcraft.unstable.ai.game.SharedAIKeys.*;

public class AiAttackerBlockEntity extends AiPlaneBase implements
    IAttackerContext
{
    public static Address<IPath> CURRENT_CRUISE = new Address<>("cruiser_current_cruise", IPath.class);


    private double cruiseRadius = 20;
    private double cruiseVelocity = 50;
    private double shootTolerance = 7.5;
    private int fireCooldown = 0;
    private int maxFireCooldown = 60;
    private final AirAttackerTargetSelector selector = new AirAttackerTargetSelector(this);


    private boolean db_fireArrow = true;

    public boolean db_fireArrow() {
        return db_fireArrow;
    }

    public void setDb_fireArrow(boolean db_fireArrow) {
        this.db_fireArrow = db_fireArrow;
    }

    public double fireRate() {
        return maxFireCooldown;
    }

    public void setFireRate(double fireRate) {
        this.maxFireCooldown = (int)fireRate;
    }

    public AiAttackerBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);


        registerDouble(this::shootTolerance, this::setShootTolerance, TOL);
        registerDouble(this::cruiseRadius, this::setCruiseRadius, RAD);
        registerBoolean(this::db_fireArrow, this::setDb_fireArrow, ARROW);
        registerDouble(this::fireRate, this::setFireRate, SharedAIKeys.FIRE_RATE);


        storage.set(ATTACKER_CONTEXT, this);
        storage.set(AIR_COMMON, this);
        storage.set(AWARENESS, awareness);
        storage.set(CIRCLE_CONTEXT, ICircleContext.ofAttacker(this));
        storage.set(ANCHOR_CONTEXT, IAnchorContext.of(this));

    }

    @Override
    protected BehaviorTree constructAI() {
        Node root = new Parallel(ParallelPolicy.SUCCEED_ON_ALL).addChild(
                new Always(
                        new ThenUntilElse(
                                new AttackerPullEnterCondition(),
                                new AvoidCollideAction(),
                                new AttackerPullExitCondition(),
                                new ThenUntilElse(
                                        new AttackEnterCondition(),
                                        new Sequence().addChild(
                                                new MakeAdjustPathAction(),
                                                new PathAlongAction(),
                                                new MakeStrikePathAction(),
                                                new PathAlongAction(),
                                                new TossAction()
                                        ),
                                        new Inverter(new AttackEnterCondition()),
                                        new ThenUntilElse(
                                                new FighterEnterCondition(),
                                                new ThenUntilElse(
                                                        new FighterPullEnterCondition(),
                                                        new PivotUpAction(),
                                                        new FighterPullExitCondition(),
                                                        new ThenUntilElse(
                                                                new AttackerEvadeEnterCondition(),
                                                                new PivotAwayAction(),
                                                                new AttackerEvadeExitCondition(),
                                                                new PivotToAction()
                                                        )
                                                ),
                                                new Inverter(new FighterEnterCondition()),
                                                new CirclingAction()
                                        )
                                )
                        )
                ),
                new AnchorAction(),
                new AttackerAwarenessAction(),
                new AirFireAction(),
                new SuicideAction()
        );
        return new BehaviorTree(root);
    }


    public double shootTolerance() {
        return shootTolerance;
    }

    @Override
    public double fireCooldown() {
        return fireCooldown;
    }

    @Override
    public void fireAt(Vector3dc direction) {
        if(isClientSide())return;
        if(fireCooldown != 0)return;
        fireCooldown = maxFireCooldown;
        Objects.requireNonNull(level);
        Vector3dc p = readSelf().position();
        Vector3dc front = readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1));
        Vector3dc spawn = p.fma(10.0, front, new Vector3d());
        Projectile ap = db_fireArrow ?
            new Arrow(level, 0, 0, 0)
        :
            new APHECannonRocketProjectile(CBCMSEntityTypes.APHE_CANNON_ROCKET.get(), level);
        ap.setNoGravity(true);
        ap.setPos(toMinecraft(spawn));
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


//    public void debug_renderPathUnsafe(){
//        IPath path = storage.get(CURRENT_CRUISE);
//        if(path == null)return;
//        double delta = path.length() / 30;
//        for(int i = 0; i < 30; i++){
//            Vector3dc p = path.point(i * delta);
//            ClientOutliner.drawOutline(
//                    toMinecraft(MathUtils.centerWithRadius(p, 1)),
//                    Color.RED.getRGB(),
//                    "debug_path" + getBlockPos() + i,
//                    1,
//                    1f / 16
//            );
//        }
//
//    }

    private boolean useDebugTarget(){
        return !debugTargetName.isEmpty();
    }

    public @NotNull CruiseController controller(){
        return cruiseController;
    }

    @Override
    public @NotNull Vector3dc getGroundTarget() {
        return useDebugTarget() ?
                Optional.ofNullable(debug_getTarget()).orElseGet(this::below):
                Optional.ofNullable(selector.getPositionOf(selector.findGroundTarget())).orElseGet(this::below)
                ;
    }

    @Override
    public @NotNull Vector3dc getGroundTargetVelocity() {
        return useDebugTarget() ?
                Optional.ofNullable(debug_getTargetVelocity()).orElseGet(Vector3d::new):
                Optional.ofNullable(selector.getVelocityOf(selector.findGroundTarget())).orElseGet(Vector3d::new)
                ;
    }

    @Override
    public Vector3dc getAirTarget() {
        return useDebugTarget() ?
                debug_getTarget() :
                Optional.ofNullable(selector.getPositionOf(selector.findAirTarget())).orElse(getPosition())
                ;
    }

    @Override
    public Vector3dc getAirTargetVelocity() {
        return useDebugTarget() ?
                debug_getTargetVelocity() :
                Optional.ofNullable(selector.getVelocityOf(selector.findAirTarget())).orElse(new Vector3d())
                ;
    }

    @Override
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
        return readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1)); // assuming facing +z
    }

    @Override
    public Quaterniondc getRotation() {
        return readSelf().quaternion();
    }

    @Override
    public boolean noGroundTarget() {
        return useDebugTarget() ? debug_getTarget() == null : selector.findGroundTarget() == -1L;
    }

    @Override
    public boolean hasAirThreat() {
        return useDebugTarget() ? false : selector.findAirTarget() != -1L;
    }

    public void tickCooldown(){
        fireCooldown = MathUtils.clamp(fireCooldown - 1, 0, maxFireCooldown);
    }

    public void tickTarget(){
        selector.tick();
    }

    @Override
    public void tickServer() {
        super.tickServer();
        tickAI();
        syncCruiseTarget();
        tickCooldown();
        tickTarget();
    }
}
