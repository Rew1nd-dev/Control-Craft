package com.verr1.controlcraft.unstable.ai.game.attacker;

import com.verr1.controlcraft.unstable.ai.api.IAttackerContext;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Optional;

// set an anchor position to circle around
public class AnchorAction extends Action {
    public static final Address<Vector3d> LATEST = new Address<>("latest_target", Vector3d.class);


    @Override
    protected Status perform(Blackboard blackboard) {
        IAttackerContext context = blackboard.get(AiAttackerBlockEntity.CONTEXT);
        if(context == null)return Status.RUNNING;

        if(context.noGroundTarget()){
            Vector3dc latest = blackboard.computeIfAbsent(LATEST, () -> provide(context));
            Vector3dc current = context.getPosition();
            if(xzDist(latest, current) > context.extremeRadius() * 6){
                blackboard.set(LATEST, provide(context));
            }
        }else{
            Vector3dc v = Optional.ofNullable(context.getGroundTarget()).orElseGet(() -> provide(context));
            blackboard.computeIfAbsent(LATEST, () -> provide(context)).set(v);
        }
        return Status.RUNNING;
    }

    private static Vector3d provide(IAttackerContext context) {
        Vector3d v = new Vector3d(context.getPosition());
        v.y = context.height();
        return v;
    }

    private static double xzDist(Vector3dc a, Vector3dc b){
        return Math.sqrt(
                (a.x() - b.x()) * (a.x() - b.x()) +
                (a.z() - b.z()) * (a.z() - b.z())
        );
    }

}
