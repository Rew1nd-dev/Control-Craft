package com.verr1.controlcraft.unstable.targeting;

import com.verr1.controlcraft.mixinducks.IEntityDuck;
import com.verr1.controlcraft.unstable.AIServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.Ship;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;

public interface IAITarget {

    @Nullable
    Vector3dc position();

    @NotNull
    Vector3dc velocity();

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
        };
    }

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
        };
    }

}
