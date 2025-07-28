package com.verr1.controlcraft.unstable.ai.game.cruiser;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.ai.game.cruiser.v1.Situation;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import com.verr1.controlcraft.unstable.valkyrienskies.context.CruiserControllerV4;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import static com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils.*;
import static com.verr1.controlcraft.utils.MathUtils.*;

public class PivotToAction extends Action {

    /*
    * Vector3dc ox = controller.rotAxis();
        Vector3dc oy = controller.yawAxis();
        Vector3dc oz = controller.twistAxis();
        Vector3dc op = situation.targetPosition().sub(situation.currentPosition(), new Vector3d());
        Vector3dc oq = projection(op, oz, oy);
        Vector3dc or = projection(oq, oz);
        Vector3dc rq = oq.sub(or, new Vector3d());
        Vector3dc rp = op.sub(or, new Vector3d());
        Vector3dc qp = op.sub(oq, new Vector3d());
        double twist_sign = qp.dot(ox) < 0 ? 1 : -1;
        double phi = rp.angle(rq); // should < 90°
        double theta = op.angle(oz);
        double dot = oq.cross(oz, new Vector3d()).dot(ox);
        boolean shouldRot = dot > 0;
        double radius = controller.radius();
        double velocity = controller.velocity();
        double twistAn = context.twistOmega() * 0.05;
        double rotateAn = velocity / radius * 0.05;

        double finalPhi = clamp(twist_sign * phi, twistAn);
        double finalTheta = shouldRot || true ? clamp(theta, 0, rotateAn) : 0;
    * */
    @Override
    protected Status perform(Blackboard blackboard) {
        CruiserBlockEntity context = blackboard.get(CruiserBlockEntity.CONTEXT);
        Situation situation = blackboard.get(CruiserBlockEntity.AWARENESS);
        if (context == null || situation == null)return Status.RUNNING;
        CruiserControllerV4 controller = context.controller();
        Vector3dc op_wc = situation.targetPosition().sub(situation.currentPosition(), new Vector3d());

        // ControlCraft.LOGGER.debug("pivoting  towards  target: {}", context.debugTargetName());
        if(situation.isInLossCone() || situation.attackScore() < 5 || situation.mayCollide()){ //|| situation.mayCollide() situation.attackScore() < 5
            return Status.FAILURE;
        }

        controller.setAction(CruiseActions.TOWARDS);
        controller.setAction(CruiseActions.TEST);
        controller.overrideTarget(op_wc); //-finalTheta

        return Status.RUNNING;
    }


}

