package com.verr1.controlcraft.unstable.ai.game.cruiser;

import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.cruiser.v1.Situation;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import static com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils.*;
import static com.verr1.controlcraft.utils.MathUtils.*;

public class FireAction extends Action {


    @Override
    protected Status perform(Blackboard blackboard) {
        CruiserBlockEntity context = blackboard.get(CruiserBlockEntity.CONTEXT);
        Situation situation = blackboard.get(CruiserBlockEntity.AWARENESS);
        if (context == null || situation == null)return Status.RUNNING;
        double tol = context.shootTolerance();
        double angle_f = situation.frontAngle();
        Vector3dc dir = situation.targetPosition().sub(situation.currentPosition(), new Vector3d());
        if(dir.length() > 1 && angle_f < Math.toRadians(tol)){
            context.fireAt(dir);
        }
        return Status.RUNNING;
    }
}
