package com.verr1.controlcraft.utils;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.mixin.accessor.ShipObjectServerWorldAccessor;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.apigame.constraints.VSConstraint;
import org.valkyrienskies.core.apigame.world.ServerShipWorldCore;
import org.valkyrienskies.mod.common.VSGameUtilsKt;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;

public class ConstraintClusterUtil {

    public final static LoadingCache<Long, Set<Long>> CLUSTER_CACHE =
        CacheBuilder.newBuilder().maximumSize(32L).concurrencyLevel(4).expireAfterAccess(2L, TimeUnit.SECONDS).build(
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

    public static Optional<ServerShip> getShipOf(long id){
        return Optional.ofNullable(vsWorld().getAllShips().getById(id));
    }

    private static ServerShipWorldCore vsWorld() {
        return Objects.requireNonNull(VSGameUtilsKt.getShipObjectWorld(ControlCraftServer.INSTANCE));
    }

    private static Optional<ShipObjectServerWorldAccessor> safeCast(){
        return Optional
                .ofNullable(vsWorld())
                //.filter(ShipObjectServerWorldAccessor.class::isInstance)
                .map(ShipObjectServerWorldAccessor.class::cast);
    }

    public static List<VSConstraint> constraintsOf(long id, Predicate<VSConstraint> filter){
        return
                safeCast()
                        .map(
                                accessor -> {
                                    var constraints = accessor.controlCraft$getConstraints();
                                    var shipIdToConstraints = accessor.controlCraft$getShipIdToConstraints();
                                    return Optional.ofNullable(shipIdToConstraints.get(id))
                                            .map(
                                                    set -> set
                                                            .stream()
                                                            .map(constraints::get)
                                                            .filter(filter)
                                                            .toList()
                                            ).orElse(List.of());

                                })
                        .orElse(List.of());
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
        HashSet<Long> clusterSet = new HashSet<>();
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
