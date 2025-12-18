package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.google.common.collect.Queues;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.ShipForcesInducer;
import org.valkyrienskies.core.api.ships.ShipPhysicsListener;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.Queue;
import java.util.function.Consumer;

public final class QueueForceInducer implements ShipPhysicsListener {
    private final Queue<Vector3dc> invForces = Queues.newConcurrentLinkedQueue();
    private final Queue<Vector3dc> invTorques = Queues.newConcurrentLinkedQueue();
    private final Queue<Vector3dc> rotForces = Queues.newConcurrentLinkedQueue();
    private final Queue<Vector3dc> rotTorques = Queues.newConcurrentLinkedQueue();
    private final Queue<ForcePos> invPosForces = Queues.newConcurrentLinkedQueue();
    private final Queue<ForcePos> rotPosForces = Queues.newConcurrentLinkedQueue();


    public static QueueForceInducer getOrCreate(@NotNull AttachmentHolder ship){
        return ship.getOrPutAttachment(QueueForceInducer.class, QueueForceInducer::new);
    }

    @Override
    public void physTick(@NotNull PhysShip physShip, @NotNull PhysLevel physLevel) {
        pollUntilEmpty(invForces, physShip::applyInvariantForce);
        pollUntilEmpty(invTorques, physShip::applyInvariantTorque);
        pollUntilEmpty(invPosForces , (pos) -> physShip.applyInvariantForceToPos(pos.force, pos.pos) );
        pollUntilEmpty(rotPosForces , (pos) -> physShip.applyRotDependentForceToPos(pos.force, pos.pos) );
        pollUntilEmpty(rotForces, physShip::applyRotDependentForce);
        pollUntilEmpty(rotTorques, physShip::applyRotDependentTorque);
    }



    private <T> void pollUntilEmpty(Queue<T> queue, Consumer<T> consumer){
        T elem;
        while (!queue.isEmpty()){
            elem = queue.poll();
            consumer.accept(elem);
        }
    }




    public void  applyInvariantForce(Vector3dc force) {
        invForces.add(force);
    }

    public void  applyInvariantTorque(Vector3dc torque) {
        invTorques.add(torque);
    }

    public void  applyInvariantForceToPos(Vector3dc force,Vector3dc pos) {
        invPosForces.add(new ForcePos(force, pos));
    }

    public void  applyRotDependentForce(Vector3dc force) {
        rotForces.add(force);
    }

    public void  applyRotDependentTorque(Vector3dc torque) {
        rotTorques.add(torque);
    }

    public void  applyRotDependentForceToPos(Vector3dc force, Vector3dc pos) {
        rotPosForces.add(new ForcePos(force, pos));
    }



    private record ForcePos(Vector3dc force, Vector3dc pos) {}
}
