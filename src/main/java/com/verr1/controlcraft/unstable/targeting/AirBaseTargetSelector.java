package com.verr1.controlcraft.unstable.targeting;

import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.ai.api.IFighterJetContext;
import com.verr1.controlcraft.unstable.util.LazyTicker;
import kotlin.Pair;
import net.minecraft.world.level.levelgen.Heightmap;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.Ship;

import java.util.Comparator;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class AirBaseTargetSelector {

    private final Set<Long> inRange = new HashSet<>();
    private final Set<Long> air = new HashSet<>();

    private long currentTarget = -1L;

    private static double maxRange = 300;
    private final IFighterJetContext self;

    private final LazyTicker updater = new LazyTicker(60, this::updateInRange);
    private final LazyTicker quickUpdater = new LazyTicker(10, this::updateAir);
    private final LazyTicker constantUpdater = new LazyTicker(5, this::updateTarget);

    public AirBaseTargetSelector(IFighterJetContext self) {
        this.self = self;
    }

    public void updateInRange(){
        inRange.clear();
        AIServer.MANAGER
                .getAllShips()
                .stream()
                .filter(s -> !AIServer.MANAGER.isInPool(s.getId()))
                .filter(s -> s.getTransform().getPositionInWorld().distance(self.getPosition()) < 300)
                .filter(s -> s.getId() != self.shipId())
                .forEach(s -> inRange.add(s.getId()));
        updateAir();
    }

    public void updateAir(){
        air.clear();
        inRange.stream()
        .map(s ->
                AIServer.MANAGER.getShipOf(s).orElse(null)
        ).filter(
                Objects::nonNull
        ).map(
                s -> new Pair<>(s.getId(), s.getTransform().getPositionInWorld())
        ).filter(
                sp -> sp.getSecond().y() - getHeight(sp.getSecond()) > 10
        ).forEach(
                sp -> air.add(sp.getFirst())
        );
    }

    public Vector3dc getVelocityOf(long id){
        return AIServer.MANAGER.getShipOf(id).map(Ship::getVelocity).orElse(new Vector3d());
    }

    public @Nullable Vector3dc getPositionOf(long id){
        return AIServer.MANAGER.getShipOf(id).map(s -> s.getTransform().getPositionInWorld()).orElse(null);
    }

    public long findTarget(){
        return air
                .stream()
                .filter(s -> !AIServer.MANAGER.isInPool(s))
                .map(s -> new Pair<>(s, computeThreatScore(
                        self.getPosition(),
                        getPositionOf(s),
                        self.getVelocity(),
                        getVelocityOf(s),
                        self.extremeRadius()
                )))
                .max(Comparator.comparingDouble(Pair::getSecond))
                .map(Pair::getFirst)
                .orElse(-1L);
    }

    public void updateTarget(){
        currentTarget = findTarget();
    }

    public long getTarget(){
        return currentTarget;
    }

    public void tick(){
        updater.tick();
        quickUpdater.tick();
        constantUpdater.tick();
    }

    /**
     * Computes the threat score for a target based on position, velocity, and self turn radius.
     * @param selfPosition Current position of the entity (x, y, z)
     * @param targetPosition Position of the target (x, y, z)
     * @param selfVelocity Velocity vector of the entity
     * @param targetVelocity Velocity vector of the target
     * @param selfRadius Turn radius of the entity
     * @return Threat score (higher means more threatening)
     */
    public static double computeThreatScore(
            Vector3dc selfPosition,
            Vector3dc targetPosition,
            Vector3dc selfVelocity,
            Vector3dc targetVelocity,
            double selfRadius
    ) {
        // Calculate relative position and distance
        Vector3d relativePos = new Vector3d(targetPosition).sub(selfPosition);
        double distance = relativePos.length();
        if(distance > maxRange)return 0;
        // Avoid division by zero
        if (distance < 1e-6) {
            distance = 1e-6;
        }

        // Calculate relative velocity
        Vector3d relativeVelocity = new Vector3d(targetVelocity).sub(selfVelocity);

        // Calculate closing speed (negative means approaching)
        Vector3d unitRelativePos = new Vector3d(relativePos).normalize();
        double closingSpeed = -relativeVelocity.dot(unitRelativePos);

        // If target is not approaching, return 0 (no threat)
        if (closingSpeed <= 0) {
            return 0.0;
        }

        // Calculate intent factor (how much target is heading toward self)
        double targetSpeed = targetVelocity.length();
        if (targetSpeed < 1e-6) {
            return 0.0; // No threat if target is stationary
        }

        Vector3d unitTargetVelocity = new Vector3d(targetVelocity).normalize();
        Vector3d toSelf = new Vector3d(selfPosition).sub(targetPosition).normalize();
        double intentFactor = Math.max(0.0, unitTargetVelocity.dot(toSelf));

        // If intent is low, return 0 (no threat)
        if (intentFactor < 0.5) {
            return 0.0;
        }

        // Compute threat score: (closingSpeed * intentFactor) / distance
        return (closingSpeed * intentFactor) / distance;
    }

    private double getHeight(Vector3dc sp){
        return self.world().getHeight(Heightmap.Types.MOTION_BLOCKING, (int)sp.x(), (int)sp.z());
    }
}
