package com.verr1.controlcraft.unstable.targeting;

import com.verr1.controlcraft.mixinducks.IEntityDuck;
import com.verr1.controlcraft.unstable.AIServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.mod.common.VSGameUtilsKt;

import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;

public interface IAITarget {

    @Nullable
    Vector3dc position();

    @NotNull
    Vector3dc velocity();

    default boolean isRemoved(){
        return false;
    }

    static IAITarget ofShip(long id){
        return new IAITarget() {
            @Override
            public Vector3dc position() {
                return AIServer.MANAGER.getShipOf(id).map(s -> s.getTransform().getPositionInWorld()).orElse(null);
            }

            @Override
            public @NotNull Vector3dc velocity() {
                return AIServer.MANAGER.getShipOf(id).map(Ship::getVelocity).orElse(new Vector3d());
            }

            @Override
            public boolean isRemoved() {
                return AIServer.MANAGER.getShipOf(id).isEmpty();
            }
        };
    }

    static IAITarget ofFixed(Vector3dc position){
        return new IAITarget() {
            @Override
            public Vector3dc position() {
                return position;
            }

            @Override
            public @NotNull Vector3dc velocity() {
                return new Vector3d();
            }

        };
    }

//    static IAITarget ofShip(long id, ServerLevel level){
//        return new IAITarget() {
//
//            private Optional<Ship> ship(){
//                return Optional.ofNullable(VSGameUtilsKt.getAllShips(level).getById(id));
//            }
//
//            @Override
//            public Vector3dc position() {
//                return ship().map(s -> s.getTransform().getPositionInWorld()).orElse(null);
//            }
//
//            @Override
//            public @NotNull Vector3dc velocity() {
//                return ship().map(Ship::getVelocity).orElse(new Vector3d());
//            }
//
//            @Override
//            public boolean isRemoved() {
//                return VSGameUtilsKt.getAllShips(level).getById(id) == null;
//            }
//        };
//    }

    static IAITarget ofEntity(Entity entity){
        return new IAITarget() {
            @Override
            public Vector3dc position() {
                return new Vector3d(entity.getX(), entity.getY(), entity.getZ());
            }

            @Override
            public @NotNull Vector3dc velocity() {
                if(entity instanceof IEntityDuck duck){
                    return toJOML(duck.controlCraft$velocityObserver());
                }
                return toJOML(entity.getDeltaMovement()).mul(0.05);
            }

            @Override
            public boolean isRemoved() {
                return entity.isRemoved();
            }
        };
    }

}
