package com.verr1.controlcraft.content.blocks;

import com.simibubi.create.content.equipment.clipboard.ClipboardCloneable;
import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkBus;
import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkPorts;
import com.verr1.controlcraft.content.valkyrienskies.attachments.Observer;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.data.ShipPhysics;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.ClientShip;
import org.valkyrienskies.core.api.ships.LoadedServerShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.apigame.world.ServerShipWorldCore;
import org.valkyrienskies.core.impl.game.ships.DummyShipWorldServer;

import javax.annotation.Nullable;
import java.util.*;

import static org.valkyrienskies.mod.common.util.VectorConversionsMCKt.toJOML;

public abstract class OnShipBlockEntity extends NetworkBlockEntity implements ClipboardCloneable
{


    public OnShipBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        buildRegistry(SharedKeys.COMPONENT_NAME)
                .withBasic(SerializePort.of(this::deviceName, this::setDeviceName, SerializeUtils.STRING))
                .withClient(ClientBuffer.STRING.get())
                .register();
    }

    public Vector3d getDirectionJOML() {
        return ValkyrienSkies.set(new Vector3d(), getDirection().getNormal());
    }

    public @NotNull Direction getDirection(){
        if(getBlockState().hasProperty(BlockStateProperties.FACING)) return getBlockState().getValue(BlockStateProperties.FACING);
        return Direction.UP;
    }

    public void setDeviceName(String name){
        if(this instanceof IPlant plant){
            plant.setName(name);
        }
    }

    public String deviceName(){
        if(this instanceof IPlant plant){
            return plant.getName();
        }
        return "";
    }

    public Optional<CimulinkPorts> linkStorage(){
        return Optional.ofNullable(getLoadedServerShip()).map(CimulinkPorts::getOrCreate);
    }



    public Vector3d getBasePosition(){
        Vector3d p_sc = ValkyrienSkies.set(new Vector3d(), getBlockPos().getCenter());
        return Optional
                .ofNullable(getShipOn())
                .map(ship -> ship
                        .getTransform()
                        .getShipToWorld()
                        .transformPosition(p_sc)
                )
                .orElse(p_sc);
    }

    public Vector3d getBaseVelocity(){
        return Optional
                .ofNullable(getShipOn())
                .map(ship ->
                {
                    ShipPhysics p = readSelf();
                    Vector3dc sv_wc = p.velocity();
                    Vector3dc sw_wc = p.omega();

                    Vector3dc s_sc = p.positionInShip();
                    Vector3dc p_sc = ValkyrienSkies.set(new Vector3d(), getBlockPos().getCenter());
                    Vector3dc r_sc = new Vector3d(p_sc).sub(s_sc);

                    Vector3dc r_wc = p.s2wTransform().transformDirection(r_sc, new Vector3d());
                    return new Vector3d(sv_wc).add(new Vector3d(sw_wc).cross(r_wc));
                })
                .orElse(new Vector3d());

    }

    @Override
    public boolean writeToClipboard(CompoundTag tag, Direction side) {
        write(tag, false);
        return true;
    }

    @Override
    public boolean readFromClipboard(CompoundTag tag, Player player, Direction side, boolean simulate) {
        read(tag, false);
        return true;
    }

    public @NotNull ShipPhysics readSelf(){
        if(level == null || level.isClientSide)return ShipPhysics.EMPTY;

        return Optional
                .ofNullable(getLoadedServerShip())
                .filter(s -> !s.isStatic())
                .map(Observer::getOrCreate)
                .map(Observer::read)
                .orElseGet(() -> ShipPhysics.of(getLoadedServerShip()));
    }

    public boolean isOnShip(){
        return getShipOn() != null;
    }

    public @Nullable LoadedServerShip getLoadedServerShip(){
        if(level == null || level.isClientSide)return null;
        return Optional
                .of(ValkyrienSkies.getShipWorld(level.getServer()))
                .map((shipWorld -> shipWorld.getLoadedShips().getById(getShipOrGroundID()))).orElse(null);
    }

    @OnlyIn(Dist.CLIENT)
    public @Nullable ClientShip getClientShip(){
        if(level == null || !level.isClientSide)return null;
        return Optional
                .of(ValkyrienSkies.getShipWorld(Minecraft.getInstance()))
                .map(shipWorld -> shipWorld.getLoadedShips().getById(getShipOrGroundID())).orElse(null);
    }

    public @Nullable Ship getShipOn(){
        return ValkyrienSkies.getShipManagingBlock(level, getBlockPos());
    }

    public Quaterniondc getSelfShipQuaternion(){
        Quaterniond q = new Quaterniond();
        Optional
            .ofNullable(getShipOn())
            .ifPresent(
                    serverShip -> serverShip
                            .getTransform()
                            .getShipToWorldRotation()
                            .get(q)
            );
        return q;
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        tickBus();
    }

    @Override
    public String getClipboardKey() {
        return this.getClass().getSimpleName();
    }

    protected void tickBus(){
        if(this instanceof IPlant plant){
            NamedComponent device = plant.plant();
            Optional.ofNullable(getLoadedServerShip())
                    .map(CimulinkBus::getOrCreate)
                    .ifPresent(bus -> bus.activate(getWorldBlockPos(), device, device.name()));
        }

    }

    public String getDimensionID(){
        return Optional
                .ofNullable(level)
                .map(ValkyrienSkies::getDimensionId)
                .orElse("");
    }



    public long getGroundBodyID(){
        return Optional
                .ofNullable(level)
                .filter(ServerLevel.class::isInstance)
                .map(ServerLevel.class::cast)
                .map(ValkyrienSkies::getShipWorld)
                .filter(sw -> !(sw instanceof DummyShipWorldServer))
                .map(ServerShipWorldCore::getDimensionToGroundBodyIdImmutable)
                .map(m -> m.get(getDimensionID()))
                .orElse(-1L);
    }

    public long getShipOrGroundID(){
        return Optional
                .ofNullable(getShipOn())
                .map(Ship::getId)
                .orElse(getGroundBodyID());

    }



}
