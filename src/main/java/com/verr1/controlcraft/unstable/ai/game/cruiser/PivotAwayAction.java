package com.verr1.controlcraft.unstable.ai.game.cruiser;


import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.cruiser.v1.Situation;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import com.verr1.controlcraft.unstable.valkyrienskies.context.CruiserControllerV4;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.awt.*;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;
import static com.verr1.controlcraft.unstable.ai.game.cruiser.v1.EscapeAction.EVADE_TARGET_ANGLE;

public class PivotAwayAction extends Action {

    /*
    * Vector3dc op_wc = situation.targetPosition().sub(situation.currentPosition(), new Vector3d());
        double randAn = situation.peekRandom(0);
        double randPhi = randAn * Math.PI * 2;
        double radius = controller.radius();
        Vector3dc randDir = new Vector3d(Math.cos(randPhi), 0, Math.sin(randPhi)).mul(radius * 12);
        ControlCraft.LOGGER.debug("pivoting away from target: {}", context.debugTargetName());
        controller.setAction(CruiseActions.AWAY);
        Vector3dc target = op_wc.add(randDir, new Vector3d());
    * */

    @Override
    protected Status perform(Blackboard blackboard) {
        CruiserBlockEntity context = blackboard.get(CruiserBlockEntity.CONTEXT);
        Situation situation = blackboard.get(CruiserBlockEntity.AWARENESS);
        if (context == null || situation == null)return Status.FAILURE;
        CruiserControllerV4 controller = context.controller();
        // ControlCraft.LOGGER.debug("pivoting away from target: {}", context.debugTargetName());
        Vector3dc targetP = situation.targetPosition();
        Vector3dc targetV = situation.targetVelocity();
        Vector3dc currentP = context.getPosition();
        Vector3dc currentV = context.getVelocity();

        // context.controller().setEscaping(true);

        double idealV = context.controller().velocity();
        double idealR = context.controller().radius();

        double safeDistance = Math.max(targetV.length(), idealV) * idealR / idealV;
        double randAn = situation.peekRandom(4);
        double randSi = situation.peekRandom(1);

        double evadeAngle = blackboard.computeIfAbsent(EVADE_TARGET_ANGLE, () -> 0.0);
        double evadeOmega = MathUtils.lerp(randAn, 5, 8) * randSi > 0.5 ? 1 : -1;
        double evadeAngle_J = randAn * Math.PI * 2;
        blackboard.set(EVADE_TARGET_ANGLE, MathUtils.radianReset(evadeAngle + evadeOmega * 0.05));

        Vector3dc back = MathUtils.safeNormalize(targetV.negate(new Vector3d()), new Vector3d(0, 1, 0));
        Vector3dc tang = MathUtils.safeNormalize(back.cross(new Vector3d(0, 1, 0), new Vector3d()), new Vector3d(1, 0, 0));
        Vector3dc evadeTarget_0 = targetP.fma(safeDistance / 3, back, new Vector3d());
        Vector3dc evadeTarget_1 = tang.mul(4 * safeDistance, new Vector3d()).rotateAxis(evadeAngle, back.x(), back.y(), back.z(), new Vector3d());
        Vector3dc finalTarget = evadeTarget_1.add(evadeTarget_0, new Vector3d());
        Vector3dc finalDirection = finalTarget.sub(currentP, new Vector3d());
//        double randPhi = randAn * Math.PI * 2;
//        Vector3dc randDir = new Vector3d(Math.cos(randPhi), 0, Math.sin(randPhi)).mul(safeDistance * 4);
//        Vector3dc finalTarget = targetP.add(randDir, new Vector3d());
//        Vector3dc finalDirection = finalTarget.sub(currentP, new Vector3d());

//        ClientOutliner.drawOutline(
//                toMinecraft(MathUtils.centerWithRadius(currentP, 6)),
//                Color.RED.getRGB(),
//                "debug_evade_target" + context.getBlockPos(),
//                4.0,
//                1f / 16
//        );


        controller.setAction(CruiseActions.AWAY);
        // controller.setAction(CruiseActions.TEST);
        controller.overrideTarget(finalDirection);
        Vector3dc actualDirection = targetP.sub(currentP, new Vector3d());
        if(
                   !situation.isInLossCone()
                 || situation.distance() > 3 * safeDistance
                 // || situation.heuristicWindow() > 0.7

        ){ // actualDirection.length() > 2 * safeDistance || !situation.peekDecision()
            situation.resetAttackScore();
            return Status.SUCCESS;
        }

        return Status.RUNNING;
    }


}

