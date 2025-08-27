package com.verr1.controlcraft.unstable.ai.game.attacker;

import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.api.IFighterJetContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;
import com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Optional;

public class AttackerAwarenessAction extends Action {

    @Override
    protected Status perform(Blackboard blackboard){
        IAttackerContext context = blackboard.get(SharedAIKeys.ATTACKER_CONTEXT);
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        awareness.tick();
        awareness.overrideDual(context.getPosition(), context.getVelocity());
        if(context.noGroundTarget() && context.hasAirThreat()){
            return fighter(context, awareness);
        }else{
            return attacker(context, awareness);
        }

    }

    protected Status fighter(IAttackerContext context, AirBaseAwareness awareness) {

        if (context == null || awareness == null || !context.hasAirThreat())return Status.RUNNING;

        Vector3dc targetPNullable = context.getAirTarget();
        Vector3dc targetV =
                Optional.ofNullable(context.getAirTargetVelocity())
                        .orElse(new Vector3d());

        if(targetPNullable == null)return Status.RUNNING;
        Vector3dc currentP = context.getPosition();
        Vector3dc currentV = context.getVelocity();
        Vector3dc fireP = currentP.fma(10, context.getHeading(), new Vector3d());
        Vector3dc aim = AIControlUtils.aimPredict(targetPNullable, targetV, fireP, 140);
        Vector3dc finalTarget = aim == null ? targetPNullable : aim;
        awareness.overrideTarget(
                finalTarget,
                targetV,
                finalTarget,
                context.getHeading()
        );

        return Status.RUNNING;
    }

    protected Status attacker(IAttackerContext context, AirBaseAwareness awareness) {

        if (context == null || awareness == null)return Status.RUNNING;


        awareness.overrideTarget(
                context.getGroundTarget(),
                context.getGroundTargetVelocity(),
                context.getCruiseTarget(),
                context.getHeading()
        );


        return Status.RUNNING;
    }

}
