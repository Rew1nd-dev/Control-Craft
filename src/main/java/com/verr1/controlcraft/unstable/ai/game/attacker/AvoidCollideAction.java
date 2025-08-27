package com.verr1.controlcraft.unstable.ai.game.attacker;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class AvoidCollideAction extends Action {


    @Override
    protected Status perform(Blackboard blackboard) {
        String msg = "Avoiding collision--";
        IAttackerContext context = blackboard.get(SharedAIKeys.ATTACKER_CONTEXT);
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        if(context == null)return Status.FAILURE;
        Quaterniondc q = context.getRotation();
        Vector3d aim = new Vector3d();
        double angle =  2 * Math.PI * awareness.peekRandom(2);
        Vector3dc head = MathUtils.safeNormalize(new Vector3d(context.getHeading()).setComponent(1, 0), new Vector3d(1,0,0));
        if(awareness.currentHeight() > context.cruiseRadius() * 1.5){
            if(awareness.currentObstacleDistance() / context.cruiseVelocity() > 2){
                return Status.SUCCESS;
            }
            msg += "pull-away:";
            aim.set(q.transform(new Vector3d(Math.cos(angle), Math.sin(angle), -5)));

        }else {
            msg += "pull-up:";
            aim.set(new Vector3d(head.x() * 3, 15, head.z() * 3));
        }
        // ControlCraft.LOGGER.info(msg + aim);
        context.controller().overrideTarget(aim);
        return Status.RUNNING;

    }
}
