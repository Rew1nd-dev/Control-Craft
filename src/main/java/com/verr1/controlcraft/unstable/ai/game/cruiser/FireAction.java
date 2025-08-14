package com.verr1.controlcraft.unstable.ai.game.cruiser;

import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class FireAction extends Action {


    @Override
    protected Status perform(Blackboard blackboard) {
        IAirContext context = blackboard.get(SharedAIKeys.CONTEXT);
        AirAwareness awareness = blackboard.get(CruiserBlockEntity.AWARENESS);
        if (context == null || awareness == null)return Status.RUNNING;
        double tol = context.shootTolerance();
        double angle_f = awareness.frontAngle();
        Vector3dc dir = awareness.targetPosition().sub(awareness.currentPosition(), new Vector3d());
        if(dir.length() > 1 && angle_f < Math.toRadians(tol)){
            context.fireAt(dir);
        }
        return Status.RUNNING;
    }
}
