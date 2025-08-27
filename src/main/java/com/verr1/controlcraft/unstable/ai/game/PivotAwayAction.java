package com.verr1.controlcraft.unstable.ai.game;


import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.ai.api.IAirController;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.core.nodes.Interruptible;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;
import static com.verr1.controlcraft.unstable.ai.game.SharedAIKeys.EVADE_TARGET_ANGLE;
import static com.verr1.controlcraft.unstable.ai.game.attacker.PathAlongAction.unsafeRenderPosition;

public class PivotAwayAction extends Action implements Interruptible {



    @Override
    protected Status perform(Blackboard blackboard) {
        IAirContext context = blackboard.get(SharedAIKeys.AIR_COMMON);
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        if (context == null || awareness == null)return Status.FAILURE;
        IAirController controller = context.controller();
        // ControlCraft.LOGGER.debug("pivoting away from target");
        Vector3dc targetP = awareness.targetPosition();
        Vector3dc targetV = awareness.targetVelocity();
        Vector3dc currentP = context.getPosition();
        Vector3dc currentV = context.getVelocity();


        double safeDistance = awareness.safeDistance();
        double randAn = awareness.peekRandom(4);
        double randSi = awareness.peekRandom(1);

        double evadeAngle = blackboard.computeIfAbsent(EVADE_TARGET_ANGLE, () -> 0.0);
        double evadeOmega = MathUtils.lerp(randAn, 5, 8) * randSi > 0.5 ? 1 : -1;
        double evadeAngle_J = randAn * Math.PI * 2;
        blackboard.set(EVADE_TARGET_ANGLE, MathUtils.radianReset(evadeAngle + evadeOmega * 0.05));

        Vector3dc back = MathUtils.safeNormalize(targetV.negate(new Vector3d()), new Vector3d(0, 1, 0));
        Vector3dc tang = MathUtils.safeNormalize(back.cross(new Vector3d(0, 1, 0), new Vector3d()), new Vector3d(1, 0, 0));
        Vector3dc evadeTarget_0 = targetP.fma(safeDistance * 3, back, new Vector3d());
        Vector3dc evadeTarget_1 = tang.mul(4 * safeDistance, new Vector3d()).rotateAxis(evadeAngle, back.x(), back.y(), back.z(), new Vector3d());
        Vector3dc finalTarget = evadeTarget_1.add(evadeTarget_0, new Vector3d());
        Vector3dc finalDirection = finalTarget.sub(currentP, new Vector3d());
        controller.overrideTarget(finalDirection);


        unsafeRenderPosition(finalTarget, this);

        return Status.RUNNING;
    }


    @Override
    public void interrupt(Blackboard blackboard) {
        AirBaseAwareness awareness = blackboard.get(SharedAIKeys.AWARENESS);
        if (awareness == null)return;
        // awareness.resetAttackScore();
    }
}

