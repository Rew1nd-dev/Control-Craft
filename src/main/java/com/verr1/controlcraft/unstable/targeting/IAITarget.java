package com.verr1.controlcraft.unstable.targeting;

import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.mixinducks.IEntityDuck;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.utils.VSGetterUtils;
import net.minecraft.core.BlockPos;
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
import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

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

    static IAITarget ofShipLocation(Vector3dc blockPositionShip, ServerLevel level){
        return new IAITarget() {
            final BlockPos blockPos = BlockPos.containing(toMinecraft(blockPositionShip));

            ServerShip ship(){
                return AIServer.MANAGER.getShipAt(WorldBlockPos.of(level, blockPos)).orElse(null);
            }


            @Override
            public @Nullable Vector3dc position() {
                ServerShip ship = ship();
                if(ship == null)return blockPositionShip;
                return ship.getShipToWorld().transformPosition(blockPositionShip, new Vector3d());
            }

            @Override
            public @NotNull Vector3dc velocity() {
                return _velocity(blockPositionShip, level);
            }
        };
    }

    @NotNull
    static Vector3dc _velocity(Vector3dc blockPositionShip, ServerLevel level){
        BlockPos blockPos = BlockPos.containing(toMinecraft(blockPositionShip));
        ServerShip ship = AIServer.MANAGER.getShipAt(WorldBlockPos.of(level, blockPos)).orElse(null);;
        if(ship == null)return new Vector3d();
        Vector3dc sv_wc = ship.getVelocity();
        Vector3dc sw_wc = ship.getOmega();
        Vector3dc r_sc = new Vector3d(blockPositionShip).sub(ship.getTransform().getPositionInShip());
        Vector3dc r_wc = ship.getShipToWorld().transformDirection(r_sc, new Vector3d());
        return new Vector3d(sv_wc).add(new Vector3d(sw_wc).cross(r_wc));
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

            @Override
            public boolean isRemoved() {
                return entity.isRemoved();
            }
        };
    }

}
