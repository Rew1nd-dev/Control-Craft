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

public class PivotUpAction extends Action {

    @Override
    protected Status perform(Blackboard blackboard) {
        // ControlCraft.LOGGER.info("Pivoting up");

        IAirContext context = blackboard.get(SharedAIKeys.AIR_COMMON);
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        if (context == null || awareness == null)return Status.RUNNING;
        IAirController controller = context.controller();

        double angle =  2 * Math.PI * awareness.peekRandom(2);
        Vector3dc up = new Vector3d(Math.cos(angle), 5, Math.sin(angle));


        controller.overrideTarget(up);

        return Status.RUNNING;
    }

}
