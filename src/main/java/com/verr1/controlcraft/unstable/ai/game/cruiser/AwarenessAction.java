package com.verr1.controlcraft.unstable.ai.game.cruiser;

import com.verr1.controlcraft.unstable.ai.api.IFighterJetContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Optional;


public class AwarenessAction extends Action {

    @Override
    protected Status perform(Blackboard blackboard) {
        IFighterJetContext context = blackboard.get(SharedAIKeys.FIGHTER_CONTEXT);
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);

        if (context == null || awareness == null)return Status.RUNNING;

        double ratio = awareness
                .state()
                .newY(context.getPosition().y())
                .cruiseRatio();

        context.controller().setVelocity(context.cruiseVelocity());

        Vector3dc targetPNullable = context.getTargetPosition();
        Vector3dc targetVNullable =
                Optional.ofNullable(context.getTargetVelocity())
                .orElse(new Vector3d());

        if(targetPNullable == null)return Status.RUNNING;
        Vector3dc currentP = context.getPosition();
        Vector3dc currentV = context.getVelocity();
        Vector3dc fireP = currentP.fma(10, context.getHeading(), new Vector3d());
        Vector3dc aim = AIControlUtils.aimPredict(targetPNullable, targetVNullable, fireP, 140);
        Vector3dc finalTarget = aim == null ? targetPNullable : aim;
        awareness.overrideDual(
                finalTarget,
                targetVNullable,
                currentP,
                currentV,
                context.getTargetPosition(),
                context.getHeading()
        );

        awareness.tick();

        return Status.RUNNING;
    }



}
