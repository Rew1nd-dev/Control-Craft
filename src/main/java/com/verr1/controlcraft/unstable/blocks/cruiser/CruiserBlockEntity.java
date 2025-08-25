package com.verr1.controlcraft.unstable.blocks.cruiser;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.compact.createbigcannons.APAutocannonAccess;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.unstable.ai.api.IFighterJetContext;
import com.verr1.controlcraft.unstable.ai.core.BehaviorTree;
import com.verr1.controlcraft.unstable.ai.core.nodes.*;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.*;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.EvadeEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.EvadeExitCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.PullEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.PullExitCondition;
import com.verr1.controlcraft.unstable.blocks.AiPlaneBase;
import com.verr1.controlcraft.unstable.targeting.AirBaseTargetSelector;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Objects;
import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class CruiserBlockEntity extends AiPlaneBase implements
        IFighterJetContext
{
    private final AirBaseTargetSelector selector = new AirBaseTargetSelector(this);
    private final Vector3d noTargetAround = new Vector3d();
    private double shootTolerance = 7.5;


    public CruiserBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);


        registerDouble(this::shootTolerance, this::setShootTolerance, SharedAIKeys.TOL);

        storage.set(SharedAIKeys.AIR_COMMON, this);
        storage.set(SharedAIKeys.AWARENESS, awareness);



    }

    @Override
    protected BehaviorTree constructAI() {
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
                new SuicideAction(),
                new AwarenessAction()
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
    public boolean noTarget(){
        return debugTargetName().isEmpty() && selector.getTarget() == -1L;
    }

    public void tickIfNoTarget(){
        if(noTarget() || noTargetAround.distance(getPosition()) < 500)return;
        ControlCraft.LOGGER.info("No Target, Setting An anchor, {}", getBlockPos().toShortString());
        noTargetAround.set(getPosition());
    }

    public void fireAt(Vector3dc direction){
        if(isClientSide())return;

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


    @Override
    public void tickServer() {
        super.tickServer();
        tickAI();
        selector.tick();
        tickIfNoTarget();
        syncForAllPlayers(false, SharedAIKeys.GOAL, SharedAIKeys.PATH);
        syncCruiseTarget();
        syncNetwork();
    }


}
