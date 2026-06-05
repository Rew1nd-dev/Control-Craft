package com.verr1.controlcraft.utils;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Sets;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.ships.LoadedServerShip;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.internal.joints.VSJoint;
import org.valkyrienskies.core.internal.world.VsiServerShipWorld;
import org.valkyrienskies.mod.common.VSGameUtilsKt;
import org.valkyrienskies.mod.common.ValkyrienSkiesMod;
import org.valkyrienskies.mod.common.util.GameToPhysicsAdapter;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;

public class ConstraintClusterUtil {

    public final static LoadingCache<Long, Set<Long>> CLUSTER_CACHE =
        CacheBuilder.newBuilder().maximumSize(256L).concurrencyLevel(4).expireAfterWrite(2L, TimeUnit.SECONDS).build(
            new CacheLoader<>() {
                @Override
                public @NotNull Set<Long> load(@NotNull Long key) throws Exception {
                    return ConstraintClusterUtil.clusterOf(key);
                }
            }
        );

    public static Optional<Long> shipOf(long id){
        return Optional.ofNullable(vsWorld().getAllShips().getById(id)).map(Ship::getId);
    }

    public static String dimensionOf(long id){
        return getShipOf(id).map(ServerShip::getChunkClaimDimension).orElse("null");
    }

    public static Optional<LoadedServerShip> getShipOf(long id){
        return Optional.ofNullable(vsWorld().getLoadedShips().getById(id));
    }

    private static VsiServerShipWorld vsWorld() {
        return Objects.requireNonNull(VSGameUtilsKt.getShipObjectWorld(ControlCraftServer.INSTANCE));
    }

    private static GameToPhysicsAdapter adapter(long id) {
        return Objects.requireNonNull(ValkyrienSkiesMod.getOrCreateGTPA(dimensionOf(id)));
    }

    public static List<VSJoint> constraintsOf(long id, Predicate<VSJoint> filter){
        var constraints = adapter(id).getAllJoints();
        return Optional.ofNullable(adapter(id).getJointsFromShip(id))
                .map(
                        set -> set
                                .stream()
                                .map(constraints::get)
                                .filter(filter)
                                .toList()
                ).orElse(List.of());
    }

    public static List<Long> connectedOf(long id, Predicate<Long> filter){
        return constraintsOf(id, constraint -> true)
                .stream()
                .map(
                        constraint -> {
                            long id_0 = constraint.getShipId0();
                            long id_1 = constraint.getShipId1();
                            return id == id_0 ? id_1 : id_0;
                        })
                .filter(filter)
                .distinct()
                .toList();
    }


    public static @NotNull Set<Long> cachedClusterOf(long id){
        return CLUSTER_CACHE.getUnchecked(id);
    }

    public static @NotNull Set<Long> clusterOf(long id){
        if(shipOf(id).isEmpty())return Set.of();

        int max_depth = 1024;
        Long GROUND_BODY_ID = vsWorld().getDimensionToGroundBodyIdImmutable().get(dimensionOf(id));
        Set<Long> clusterSet = Sets.newConcurrentHashSet();
        Queue<Long> unvisited = new ArrayDeque<>(List.of(id));
        while (!unvisited.isEmpty() && max_depth > 0){
            long current = unvisited.poll();
            clusterSet.add(current);
            connectedOf(
                    current,
                    id_ -> !clusterSet.contains(id_) && !Objects.equals(id_, GROUND_BODY_ID)
            )
                    .forEach(unvisited::offer);
            if(max_depth-- == 1){
                ControlCraft.LOGGER.warn("Cluster search depth exceeded !");
            }
        }
        return clusterSet;
    }

}