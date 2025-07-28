package com.verr1.controlcraft.unstable.ai.game.cruiser;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.cruiser.v1.Situation;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Optional;


public class AwarenessAction extends Action {

    @Override
    protected Status perform(Blackboard blackboard) {
        CruiserBlockEntity context = blackboard.get(CruiserBlockEntity.CONTEXT);
        Situation situation = blackboard.get(CruiserBlockEntity.AWARENESS);

        if (context == null || situation == null)return Status.RUNNING;

        double ratio = situation
                .state()
                .newY(context.readSelf().position().y())
                .cruiseRatio();

        context.controller().setVelocity(context.cruiseVelocity() * ratio);

        Vector3dc targetPNullable = context.debug_getTarget();
        Vector3dc targetVNullable =
                Optional.ofNullable(context.debug_getTargetVelocity())
                .orElse(new Vector3d());

        if(targetPNullable == null)return Status.RUNNING;
        Vector3dc currentP = context.getPosition();
        Vector3dc currentV = context.getVelocity();
        Vector3dc fireP = currentP.fma(10, context.getHeading(), new Vector3d());
        Vector3dc aim = AIControlUtils.aimPredict(targetPNullable, targetVNullable, fireP, 140);
        Vector3dc finalTarget = aim == null ? targetPNullable : aim;
        situation.overrideDual(
                finalTarget,
                targetVNullable,
                currentP,
                currentV,
                context.getHeading()
        );

        situation.tick();

        return Status.RUNNING;
    }



}
