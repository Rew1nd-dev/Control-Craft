package com.verr1.controlcraft.foundation.api;

import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkBus;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.ships.LoadedServerShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.apigame.world.ServerShipWorldCore;
import org.valkyrienskies.core.impl.game.ships.DummyShipWorldServer;

import java.util.Optional;

public interface IOnShipBlockEntity {

    Level getLevel();

    BlockPos getBlockPos();

    default String getDimensionID(){
        return Optional
                .ofNullable(getLevel())
                .map(ValkyrienSkies::getDimensionId)
                .orElse("");
    }

    default long getGroundBodyID(){
        return Optional
                .ofNullable(getLevel())
                .filter(ServerLevel.class::isInstance)
                .map(ServerLevel.class::cast)
                .map(ValkyrienSkies::getShipWorld)
                .filter(sw -> !(sw instanceof DummyShipWorldServer))
                .map(ServerShipWorldCore::getDimensionToGroundBodyIdImmutable)
                .map(m -> m.get(getDimensionID()))
                .orElse(-1L);
    }

    default void tickBus(){
        if(this instanceof IPlant plant){
            NamedComponent device = plant.plant();
            Optional.ofNullable(getLoadedServerShip())
                    .map(CimulinkBus::getOrCreate)
                    .ifPresent(bus -> bus.activate(getWorldBlockPos(), device, device.name()));
        }

    }

    default WorldBlockPos getWorldBlockPos(){
        return WorldBlockPos.of(getLevel(), getBlockPos());
    }

    default @Nullable Ship getShipOn(){
        return ValkyrienSkies.getShipManagingBlock(getLevel(), getBlockPos());
    }

    default long getShipOrGroundID(){
        return Optional
                .ofNullable(getShipOn())
                .map(Ship::getId)
                .orElse(getGroundBodyID());

    }

    default @Nullable LoadedServerShip getLoadedServerShip(){
        if(getLevel() == null || getLevel().isClientSide)return null;
        return Optional
                .of(ValkyrienSkies.getShipWorld(getLevel().getServer()))
                .map((shipWorld -> shipWorld.getLoadedShips().getById(getShipOrGroundID()))).orElse(null);
    }


}
