package com.verr1.controlcraft.unstable.ai.game.jet;

import com.verr1.controlcraft.foundation.managers.ClientOutliner;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.cruiser.CruiseActions;
import com.verr1.controlcraft.unstable.blocks.jet.AiJetBlockEntity;
import com.verr1.controlcraft.unstable.pathing.path.IPath;
import com.verr1.controlcraft.unstable.valkyrienskies.context.CruiserControllerV4;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.awt.*;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;
import static com.verr1.controlcraft.unstable.blocks.jet.AiJetBlockEntity.CURRENT_CRUISE;

public class PathAlongAction extends Action {
    public static Address<Double> LATEST_ACCUMULATED = new Address<>("latest_acc", Double.class);

    @Override
    protected Status perform(Blackboard blackboard) {
        AiJetBlockEntity context = blackboard.get(AiJetBlockEntity.CONTEXT);
        IPath current = blackboard.get(CURRENT_CRUISE);
        if (context == null)return Status.RUNNING;
        if (current == null)return Status.FAILURE;
        CruiserControllerV4 controller = context.controller();
        Vector3dc pos = context.getPosition();
        double distance = current.closestDistanceFromStart(pos);
        double vel = context.cruiseVelocity();
        double latestAccumulated = blackboard.computeIfAbsent(LATEST_ACCUMULATED, () -> 0.0);
        double acc = distance + vel * 0.4;// Math.max(, latestAccumulated);
        blackboard.set(LATEST_ACCUMULATED, acc);
        Vector3dc lookAhead = current.point(acc); // Optional.ofNullable(context.debug_getTarget()).orElse(new Vector3d(0, 0, 0)); // current.point(acc);
        controller.setAction(CruiseActions.VIEW);
        controller.overrideTarget(lookAhead.sub(pos, new Vector3d()));

        ClientOutliner.drawOutline(
                toMinecraft(MathUtils.centerWithRadius(lookAhead, 1)),
                Color.RED.getRGB(),
                "debug_evade_target" + context.getBlockPos(),
                4.0,
                1f / 16
        );

        if (Math.abs(distance - current.length()) < 6)return Status.SUCCESS;

        return Status.RUNNING;
    }

}
