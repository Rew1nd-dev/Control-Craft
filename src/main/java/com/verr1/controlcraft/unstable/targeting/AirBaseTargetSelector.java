package com.verr1.controlcraft.unstable.targeting;

import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.util.LazyTicker;
import kotlin.Pair;
import net.minecraft.world.level.levelgen.Heightmap;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.Ship;

import java.util.*;
import java.util.stream.Collectors;

public class AirBaseTargetSelector {

    protected final Set<Long> inRange = new HashSet<>();
    protected final Set<Long> air = new HashSet<>();

    protected long currentTarget = -1L;

    protected static double maxRange = 600;
    protected final IAirContext self;

    protected final LazyTicker updater = new LazyTicker(60, this::updateInRange);
    protected final LazyTicker quickUpdater = new LazyTicker(10, this::updateAir);
    protected final LazyTicker constantUpdater = new LazyTicker(5, this::updateTarget);

    protected long cachedAirTarget = -1L;


    public AirBaseTargetSelector(IAirContext self) {
        this.self = self;
    }

    protected Optional<ServerShip> getShipOf(long id){
        return AIServer.MANAGER.getShipOf(id);
    }

    public static Vector3d dropY(Vector3dc v){
        return new Vector3d(v.x(), 0, v.z());
    }

    public void updateInRange(){
        inRange.clear();
        AIServer.MANAGER
                .getAllShips()
                .stream()
                .filter(s -> !AIServer.MANAGER.isInPool(s.getId()))
                .filter(s -> dropY(s.getTransform().getPositionInWorld()).distance(dropY(self.getPosition())) < maxRange)
                .filter(s -> s.getId() != self.shipId())
                .forEach(s -> inRange.add(s.getId()));
        updateAir();
    }

    public Set<Long> ground(){
        return inRange.stream().filter(s -> !air.contains(s)).collect(Collectors.toSet());
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

        cacheAirTarget();
    }

    public Vector3dc getVelocityOf(long id){
        return AIServer.MANAGER.getShipOf(id).map(Ship::getVelocity).orElse(new Vector3d());
    }

    public @Nullable Vector3dc getPositionOf(long id){
        return AIServer.MANAGER.getShipOf(id).map(s -> s.getTransform().getPositionInWorld()).orElse(null);
    }

    protected void cacheAirTarget(){
        cachedAirTarget = air
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

    public long findAirTarget(){
        return cachedAirTarget;
    }

    public void updateTarget(){
        currentTarget = findAirTarget();
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
     * Computes the threat score for a deploy based on position, velocity, and self turn radius.
     * @param selfPosition Current position of the entity (x, y, z)
     * @param targetPosition Position of the deploy (x, y, z)
     * @param selfVelocity Velocity vector of the entity
     * @param targetVelocity Velocity vector of the deploy
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

        // If deploy is not approaching, return 0 (no threat)
        if (closingSpeed <= 0) {
            return 0.0;
        }

        // Calculate intent factor (how much deploy is heading toward self)
        double targetSpeed = targetVelocity.length();
        if (targetSpeed < 1e-6) {
            return 0.0; // No threat if deploy is stationary
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
        return self.world().getHeight(Heightmap.Types.WORLD_SURFACE, (int)sp.x(), (int)sp.z());
    }
}
