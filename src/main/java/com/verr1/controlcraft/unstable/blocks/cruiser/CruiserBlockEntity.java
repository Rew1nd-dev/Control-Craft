package com.verr1.controlcraft.unstable.blocks.cruiser;

import com.cainiao1053.cbcmoreshells.CBCMSEntityTypes;
import com.cainiao1053.cbcmoreshells.munitions.big_cannon.aphe_cannon_rocket.APHECannonRocketProjectile;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.compact.createbigcannons.APAutocannonAccess;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.unstable.ai.api.IAirCannon;
import com.verr1.controlcraft.unstable.ai.api.IAnchorContext;
import com.verr1.controlcraft.unstable.ai.api.ICircleContext;
import com.verr1.controlcraft.unstable.ai.api.IFighterJetContext;
import com.verr1.controlcraft.unstable.ai.core.BehaviorTree;
import com.verr1.controlcraft.unstable.ai.core.nodes.*;
import com.verr1.controlcraft.unstable.ai.game.PivotAwayAction;
import com.verr1.controlcraft.unstable.ai.game.PivotToAction;
import com.verr1.controlcraft.unstable.ai.game.PivotUpAction;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.attacker.AnchorAction;
import com.verr1.controlcraft.unstable.ai.game.attacker.CirclingAction;
import com.verr1.controlcraft.unstable.ai.game.cruiser.*;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.*;
import com.verr1.controlcraft.unstable.blocks.AiPlaneBase;
import com.verr1.controlcraft.unstable.targeting.AirBaseTargetSelector;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.projectile.Arrow;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Objects;
import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;
import static com.verr1.controlcraft.unstable.ai.game.SharedAIKeys.*;
import static com.verr1.controlcraft.unstable.ai.game.SharedAIKeys.AWARENESS;
import static com.verr1.controlcraft.unstable.blocks.monitor.MonitorBlockEntity.*;

public class CruiserBlockEntity extends AiPlaneBase implements
        IFighterJetContext
{
    private final AirBaseTargetSelector selector = new AirBaseTargetSelector(this);
    private final Vector3d noTargetAround = new Vector3d();
    private double shootTolerance = 7.5;



//    private boolean db_fireArrow = true;
//
//    private int fireRate = 4;
//    private int fireCooldown = 0;


    public CruiserBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        registerDouble(this::shootTolerance, this::setShootTolerance, SharedAIKeys.TOL);
//        registerDouble(this::fireRate, this::setFireRate, SharedAIKeys.FIRE_RATE);
//        registerBoolean(this::db_fireArrow, this::setDb_fireArrow, ARROW);


        storage.set(AIR_COMMON, this);
        storage.set(FIGHTER_CONTEXT, this);
        storage.set(AWARENESS, awareness);
        storage.set(CIRCLE_CONTEXT, ICircleContext.ofFighter(this));
        storage.set(ANCHOR_CONTEXT, IAnchorContext.of(this));

    }

//    public double fireRate() {
//        return fireRate;
//    }
//
//    public void setFireRate(double fireRate) {
//        this.fireRate = (int)fireRate;
//    }
//
//    public boolean db_fireArrow() {
//        return db_fireArrow;
//    }
//
//    public void setDb_fireArrow(boolean db_fireArrow) {
//        this.db_fireArrow = db_fireArrow;
//    }

    @Override
    protected BehaviorTree constructAI() {
        Node root = new Parallel(ParallelPolicy.SUCCEED_ON_ALL).addChild(
                new Always(
                        new ThenUntilElse(
                                new FighterPullEnterCondition(),
                                new PivotUpAction(),
                                new FighterPullExitCondition(),
                                new ThenUntilElse(
                                        new FighterEnterCondition(),
                                        new ThenUntilElse(
                                                new EvadeEnterCondition(),
                                                new PivotAwayAction(),
                                                new EvadeExitCondition(),
                                                new PivotToAction()
                                        ),
                                        new Inverter(new FighterEnterCondition()),
                                        new CirclingAction()
                                )

                        )
                ),
                new FireAction(),
                new SuicideAction(),
                new AnchorAction(),
                new FighterAwarenessAction()
        );
        return new BehaviorTree(root);
    }


    public double shootTolerance() {
        return shootTolerance;
    }

    public void setShootTolerance(double shootTolerance) {
        this.shootTolerance = shootTolerance;
    }

    @Override
    public Vector3dc getTargetPosition() {
        return debugTargetName().isEmpty() ?
                (
                     Optional.ofNullable(selector.getPositionOf(selector.getTarget())).orElse(noTargetAround)
                )
                :
                debug_getTarget()
                ;
    }

    @Override
    public Vector3dc getTargetVelocity() {
        return debugTargetName().isEmpty() ? selector.getVelocityOf(selector.getTarget()) : debug_getTargetVelocity();
    }

    @Override
    public double cruiseRadius() {
        return 60;
    }

    @Override
    public boolean noTarget(){
        return debugTargetName().isEmpty() && selector.getTarget() == -1L;
    }

    public void tickIfNoTarget(){
        if(noTarget() || noTargetAround.distance(getPosition()) < 500)return;
        ControlCraft.LOGGER.info("No Target, Setting An anchor, {}", getBlockPos().toShortString());
        noTargetAround.set(getPosition());
    }

//    public void fireAt(Vector3dc direction){
//        if(isClientSide())return;
//        if(fireCooldown != 0)return;
//        fireCooldown = fireRate;
//        Objects.requireNonNull(level);
//        Vector3dc p = readSelf().position();
//        Vector3dc front = readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1));
//        Vector3dc spawn = p.fma(10.0, front, new Vector3d());
//        if(db_fireArrow){
//            Vec3 v = toMinecraft(spawn);
//            Projectile ap = new Arrow(level, v.x, v.y, v.z);
//            ap.setNoGravity(true);
//            ap.shoot(direction.x(), direction.y(), direction.z(), 9, 0);
//            level.addFreshEntity(ap);
//        }else {
//            APAutocannonAccess ap = CreateBigCannonsCompact.createAutocannonAp(level);
//            if(ap == null)return;
//            ap.setPos(toMinecraft(spawn));
//            ap.setTracer(true);
//            ap.setLifetime(40);
//            ap.shoot(direction.x(), direction.y(), direction.z(), 9, 0);
//            ap.addToLevel();
//        }
//    }


    @Override
    public void fireAt(Vector3dc target) {
        network().ifPresent(n -> n.forEachObject(IAirCannon.class, cannon -> cannon.fireAt(target)));
    }

//    public void tickCooldown(){
//        if(fireCooldown > 0)fireCooldown--;
//    }

    @Override
    public void tickServer() {
        super.tickServer();
        tickAI();
        selector.tick();
        tickIfNoTarget();
//        tickCooldown();
        syncForAllPlayers(false, SharedAIKeys.GOAL, SharedAIKeys.PATH);
        syncCruiseTarget();

    }


}
