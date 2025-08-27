package com.verr1.controlcraft.unstable.targeting;

import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import kotlin.Pair;
import org.joml.primitives.AABBic;
import org.valkyrienskies.core.api.ships.Ship;

import java.util.Comparator;
import java.util.List;

public class AirAttackerTargetSelector extends AirBaseTargetSelector
{

    private long cachedGroundTarget = -1L;

    public AirAttackerTargetSelector(IAirContext self) {
        super(self);
    }

    public static double computeVolume(AABBic aabBic){
        return (aabBic.maxX() - aabBic.minX()) *
               (aabBic.maxY() - aabBic.minY()) *
               (aabBic.maxZ() - aabBic.minZ());
    }

    @Override
    public void updateAir() {
        super.updateAir();
        cacheGroundTarget();
    }

    protected void cacheGroundTarget(){
        List<Pair<Long, Double>> db = ground().stream().filter(s -> !AIServer.MANAGER.isInPool(s))
                .map(s -> new Pair<>(
                                s,
                                getShipOf(s)
                                        .map(Ship::getShipAABB)
                                        .map(AirAttackerTargetSelector::computeVolume)
                                        .orElse(0.0)
                        )
                ).toList();
        cachedGroundTarget = ground()
                .stream()
                .filter(s -> !AIServer.MANAGER.isInPool(s))
                .map(s -> new Pair<>(
                                s,
                                getShipOf(s)
                                        .map(Ship::getShipAABB)
                                        .map(AirAttackerTargetSelector::computeVolume)
                                        .orElse(0.0)
                        )
                )
                .max(Comparator.comparingDouble(Pair::getSecond))
                .map(Pair::getFirst)
                .orElse(-1L);
    }

    public long findGroundTarget(){
        return cachedGroundTarget;

    }

}
