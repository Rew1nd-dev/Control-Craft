package com.verr1.controlcraft.content.blocks.camera;

import com.verr1.controlcraft.unstable.targeting.IAITarget;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.Ship;

import java.util.Optional;

public class CameraLocked implements IAITarget {
    private IAITarget wrapped = null;

    private Vector3dc latestQueryPosition = new Vector3d();

    public LockStatus status() {
        return lockStatus;
    }

    private LockStatus lockStatus = LockStatus.NO_TARGET;

    @Override
    public @NotNull Vector3dc position() {
        latestQueryPosition = Optional.ofNullable(wrapped).map(IAITarget::position).orElse(latestQueryPosition);
        return latestQueryPosition;
    }

    @Override
    public @NotNull Vector3dc velocity() {
        return Optional.ofNullable(wrapped).map(IAITarget::velocity).orElse(new Vector3d());
    }

    public void setAsEntity(Entity player){
        lockStatus = LockStatus.ENTITY;
        wrapped = IAITarget.ofEntity(player);
    }

    public void setAsShip(Ship ship){
        lockStatus = LockStatus.SHIP;
        wrapped = IAITarget.ofShip(ship.getId());
    }

    public void setAsPosition(Vector3dc position){
        lockStatus = LockStatus.FIXED;
        wrapped = IAITarget.ofFixed(position);
    }

    public void setAsShipLocation(Vector3dc position, ServerLevel level){
        lockStatus = LockStatus.SHIP_LOCATION;
        wrapped = IAITarget.ofShipLocation(position, level);
    }

    public void setWrapped(IAITarget wrapped){
        lockStatus = LockStatus.WRAPPED;
        this.wrapped = wrapped;
    }


    public void clear(){
        wrapped = null;
        lockStatus = LockStatus.NO_TARGET;
    }

    public enum LockStatus {
        ENTITY,
        SHIP,
        SHIP_LOCATION,
        FIXED,
        WRAPPED,
        NO_TARGET
    }
}
