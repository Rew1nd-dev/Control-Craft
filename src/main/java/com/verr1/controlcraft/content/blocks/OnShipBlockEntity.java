package com.verr1.controlcraft.content.blocks;

import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkBus;
import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkPorts;
import com.verr1.controlcraft.content.valkyrienskies.attachments.FlapForceInducer;
import com.verr1.controlcraft.content.valkyrienskies.attachments.Observer;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.data.ShipPhysics;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.VSMathUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.joml.primitives.AABBdc;
import org.joml.primitives.AABBic;
import org.valkyrienskies.core.api.ships.ClientShip;
import org.valkyrienskies.core.api.ships.LoadedServerShip;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.apigame.world.ServerShipWorldCore;
import org.valkyrienskies.core.impl.game.ships.DummyShipWorldServer;

import javax.annotation.Nullable;
import java.util.*;

import static com.simibubi.create.content.kinetics.base.DirectionalAxisKineticBlock.AXIS_ALONG_FIRST_COORDINATE;
import static com.simibubi.create.content.kinetics.saw.SawBlock.FLIPPED;
import static org.valkyrienskies.mod.common.util.VectorConversionsMCKt.toJOML;

public abstract class OnShipBlockEntity extends NetworkBlockEntity
{


    public OnShipBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        buildRegistry(SharedKeys.COMPONENT_NAME)
                .withBasic(SerializePort.of(this::deviceName, this::setDeviceName, SerializeUtils.STRING))
                .withClient(ClientBuffer.STRING.get())
                .register();
    }

    public Vector3d positionModel() {
        return ValkyrienSkies.toJOML(getBlockPos().getCenter());
    }

    public Vector3d position() {
        return readSelf().s2wTransform().transformPosition(positionModel());
    }

    public Vector3d positionCenterModel(){
        return new Vector3d(readSelf().positionInShip());
    }

    public Vector3d positionCenter(){
        return new Vector3d(readSelf().position());
    }

    public double mass() {
        return Optional
            .ofNullable(getLoadedServerShip())
            .map(s -> s.getInertiaData().getMass())
            .orElse(readSelf().mass());
    }

    public Vector3d frontLocal() {
        return ValkyrienSkies.set(new Vector3d(), getDirection().getNormal());
    }

    public Vector3d front() {
        return readSelf().s2wTransform().transformDirection(frontLocal());
    }

    public Vector3dc leftLocal() {
        return VSMathUtils.toJOML(leftDirection());
    }

    public Vector3d left() {
        return readSelf().s2wTransform().transformDirection(leftLocal(), new Vector3d());
    }

    public Vector3d upLocal(){
        return frontLocal().cross(leftLocal()).normalize();
    }

    public Vector3d up(){
        return readSelf().s2wTransform().transformDirection(upLocal());
    }

    public @NotNull Direction getDirection() {
        BlockState state = this.getBlockState();
        return state.hasProperty(BlockStateProperties.FACING) ? state.getValue(BlockStateProperties.FACING)
            : state.hasProperty(BlockStateProperties.HORIZONTAL_FACING)
            ? state.getValue(BlockStateProperties.HORIZONTAL_FACING)
            : Direction.SOUTH;
    }

    public Direction leftDirection() {
        BlockState state = getBlockState();
        if (state.hasProperty(AXIS_ALONG_FIRST_COORDINATE)) {
            Direction direction = getDirection();
            boolean alignFirst = state.getValue(AXIS_ALONG_FIRST_COORDINATE);

            boolean flipped = false;
            if (state.hasProperty(FLIPPED)) {
                flipped = state.getValue(FLIPPED);
            }

            Direction d0 = switch (direction) {
                case SOUTH, NORTH -> alignFirst ? Direction.WEST : Direction.UP;
                case EAST -> alignFirst ? Direction.UP : Direction.SOUTH;
                case WEST -> alignFirst ? Direction.UP : Direction.NORTH;
                case UP, DOWN -> alignFirst ? Direction.WEST : Direction.NORTH;
            };

            return flipped ? d0.getOpposite() : d0;

        } else {
            return VSMathUtils.left(getDirection());
        }

    }

    public Vector3d geometricPositionModel() {
        AABBic aabb = aabbModel();
        if (aabb == null)
            return positionModel();
        return aabb.center(new Vector3d());
    }

    public Vector3d geometricPosition() {
        return readSelf().s2wTransform().transformPosition(geometricPositionModel());
    }



    public @Nullable AABBic aabbModel() {
        return Optional.ofNullable(getShipOn()).map(Ship::getShipAABB).orElse(null);
    }

    public @Nullable AABBdc aabb() {
        return Optional.ofNullable(getShipOn()).map(Ship::getWorldAABB).orElse(null);
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

    public @NotNull ShipPhysics readSelf(){
        if(level == null || level.isClientSide){
            return ShipPhysics.of(getShipOn());
        }

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

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        tickBus();
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

    public @Nullable Vector3dc[] debug_lastTickFlapControls(){
        ServerShip ship = getLoadedServerShip();
        if(ship == null)return null;
        return FlapForceInducer.getOrCreate(ship).lastTickControl();
    }

}
