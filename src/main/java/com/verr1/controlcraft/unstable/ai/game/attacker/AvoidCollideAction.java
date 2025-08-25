package com.verr1.controlcraft.unstable.ai.game.attacker;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class AvoidCollideAction extends Action {


    @Override
    protected Status perform(Blackboard blackboard) {
        String msg = "Avoiding collision--";
        IAttackerContext context = blackboard.get(AiAttackerBlockEntity.CONTEXT);
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        if(context == null)return Status.FAILURE;
        Quaterniondc q = context.getRotation();
        Vector3d aim = new Vector3d();
        double angle =  2 * Math.PI * awareness.peekRandom(2);

        if(awareness.safeHeight()){
            if(awareness.safeObstacle()){
                return Status.SUCCESS;
            }
            msg += "pull-away";
            aim.set(q.transform(new Vector3d(Math.cos(angle), Math.sin(angle), -5)));

        }else {
            msg += "pull-up";
            aim.set(new Vector3d(Math.cos(angle), 5, Math.sin(angle)));
        }
        ControlCraft.LOGGER.info(msg);
        context.controller().overrideTarget(aim);
        return Status.RUNNING;

    }
}
