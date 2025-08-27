package com.verr1.controlcraft.unstable.ai.game;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.ai.api.IAirController;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class PivotToAction extends Action {


    @Override
    protected Status perform(Blackboard blackboard) {
        IAirContext context = blackboard.get(SharedAIKeys.AIR_COMMON);
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        if (context == null || awareness == null)return Status.RUNNING;
        IAirController controller = context.controller();
        Vector3dc op_wc = awareness.targetPosition().sub(awareness.currentPosition(), new Vector3d());

        // ControlCraft.LOGGER.debug("pivoting  towards  target");
//        if(situation.isInLossCone() || situation.attackScore() < 5 || situation.mayCollide()){ //|| situation.mayCollide() situation.attackScore() < 5
//            return Status.FAILURE;
//        }

//        controller.setAction(CruiseActions.TOWARDS);
//        controller.setAction(CruiseActions.VIEW);
        controller.overrideTarget(op_wc); //-finalTheta

        return Status.RUNNING;
    }


}

