package com.verr1.controlcraft.unstable.ai.game.cruiser.v1;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.managers.ClientOutliner;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.awt.*;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class EscapeAction extends Action {

    public static Address<Double> EVADE_TARGET_ANGLE = new Address<>("evade_target_angle", Double.class);

    @Override
    protected Status perform(Blackboard blackboard) {
        CruiserBlockEntity context = blackboard.get(CruiserBlockEntity.CONTEXT);
        Situation situation = blackboard.get(CruiserBlockEntity.AWARENESS);
        if (context == null || situation == null)return Status.RUNNING;
        ControlCraft.LOGGER.debug("escaping");
        Vector3dc targetP = situation.targetPosition();
        Vector3dc targetV = situation.targetVelocity();
        Vector3dc currentP = context.getPosition();
        Vector3dc currentV = context.getVelocity();

        // context.controller().setEscaping(true);

        double idealV = context.controller().velocity();
        double idealR = context.controller().radius();

        double safeDistance = 1.2 * targetV.length() * idealR / idealV;
        double randAn = situation.peekRandom(0);
        double randSi = situation.peekRandom(1);

        double evadeAngle = blackboard.computeIfAbsent(EVADE_TARGET_ANGLE, () -> 0.0);
        double evadeOmega = MathUtils.lerp(randAn, 5, 8) * randSi > 0.5 ? 1 : -1;
        double evadeAngle_J = randAn * Math.PI * 2;
        blackboard.set(EVADE_TARGET_ANGLE, MathUtils.radianReset(evadeAngle + evadeOmega * 0.05));

        Vector3dc back = MathUtils.safeNormalize(targetV.negate(new Vector3d()), new Vector3d(0, -1, 0));
        Vector3dc tang = MathUtils.safeNormalize(back.cross(new Vector3d(0, 1, 0), new Vector3d()), new Vector3d(1, 0, 0));
        Vector3dc evadeTarget_0 = targetP.fma(safeDistance, back, new Vector3d());
        Vector3dc evadeTarget_1 = tang.mul(safeDistance, new Vector3d()).rotateAxis(evadeAngle_J, back.x(), back.y(), back.z(), new Vector3d());
        Vector3dc finalTarget = evadeTarget_1.add(evadeTarget_0, new Vector3d());

        ClientOutliner.drawOutline(
                toMinecraft(MathUtils.centerWithRadius(finalTarget, 0.5)),
                Color.RED.getRGB(),
                "debug_evade_target",
                4.0,
                1f / 16
        );

        // context.controller().overrideTarget(finalTarget.sub(currentP, new Vector3d()), targetV);

        return Status.RUNNING;
    }
}
