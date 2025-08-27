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


public class FighterAwarenessAction extends Action {

    @Override
    protected Status perform(Blackboard blackboard) {
        IFighterJetContext context = blackboard.get(SharedAIKeys.FIGHTER_CONTEXT);
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);

        if (context == null || awareness == null)return Status.RUNNING;

        Vector3dc currentP = context.getPosition();
        Vector3dc currentV = context.getVelocity();

        awareness.overrideDual(currentP, currentV);
        awareness.tick();

        Vector3dc targetPNullable = context.getTargetPosition();
        Vector3dc targetV =
                Optional.ofNullable(context.getTargetVelocity())
                .orElse(new Vector3d());

        if(targetPNullable == null)return Status.RUNNING;


        Vector3dc fireP = currentP.fma(10, context.getHeading(), new Vector3d());
        Vector3dc aim = AIControlUtils.aimPredict(targetPNullable, targetV, fireP, 180);
        Vector3dc finalTarget = aim == null ? targetPNullable : aim;
        awareness.overrideTarget(
                finalTarget,
                targetV,
                context.getTargetPosition(),
                context.getHeading()
        );



        return Status.RUNNING;
    }



}
