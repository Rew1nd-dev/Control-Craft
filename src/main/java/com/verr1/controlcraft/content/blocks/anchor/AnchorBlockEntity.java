package com.verr1.controlcraft.content.blocks.anchor;


import com.verr1.controlcraft.content.valkyrienskies.attachments.AnchorForceInducer;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.api.IPacketHandler;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.foundation.data.logical.LogicalAnchor;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundClientPacket;
import com.verr1.controlcraft.foundation.type.RegisteredPacketType;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundServerPacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.network.NetworkEvent;

import java.util.Optional;

public class AnchorBlockEntity extends OnShipBlockEntity
    implements IPacketHandler
{

    public static NetworkKey AIR_RESISTANCE = NetworkKey.create("air_resistance");
    public static NetworkKey EXTRA_GRAVITY = NetworkKey.create("extra_gravity");
    public static NetworkKey ROTATIONAL_RESISTANCE = NetworkKey.create("rotational_resistance");
    public static NetworkKey RESISTANCE_AT_POS =  NetworkKey.create("air_resistance_at_pos");
    public static NetworkKey GRAVITY_AT_POS =  NetworkKey.create("extra_gravity_at_pos");
    public static NetworkKey SQUARE_DRAG = NetworkKey.create("square_drag");

    public double getAirResistance() {
        return airResistance;
    }

    public void setAirResistance(double airResistance) {
        this.airResistance = MathUtils.clamp(airResistance, 0, 1);
    }

    public double getExtraGravity() {
        return extraGravity;
    }

    public void setExtraGravity(double extraGravity) {
        this.extraGravity = extraGravity;
    }

    public double getRotationalResistance() {
        return rotationalResistance;
    }

    public void setRotationalResistance(double rotationalResistance) {
        this.rotationalResistance = MathUtils.clamp(rotationalResistance, 0, 1);
    }

    private double airResistance = 0;
    private double extraGravity = 0;
    private double rotationalResistance = 0;
    private boolean airResistanceAtPos = false;
    private boolean squareDrag = false;

    public boolean squareDrag() {return squareDrag;}

    public void setSquareDrag(boolean squareDrag) {this.squareDrag = squareDrag;}

    public boolean isExtraGravityAtPos() {
        return extraGravityAtPos;
    }

    public void setExtraGravityAtPos(boolean extraGravityAtPos) {
        this.extraGravityAtPos = extraGravityAtPos;
    }

    public boolean isAirResistanceAtPos() {
        return airResistanceAtPos;
    }

    public void setAirResistanceAtPos(boolean airResistanceAtPos) {
        this.airResistanceAtPos = airResistanceAtPos;
    }

    public boolean extraGravityAtPos = false;

    public AnchorBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        buildRegistry(AIR_RESISTANCE)
                .withBasic(SerializePort.of(this::getAirResistance, this::setAirResistance, SerializeUtils.DOUBLE))
                .withClient(new ClientBuffer<>(SerializeUtils.DOUBLE, Double.class)).register();

        buildRegistry(EXTRA_GRAVITY)
                .withBasic(SerializePort.of(this::getExtraGravity, this::setExtraGravity, SerializeUtils.DOUBLE))
                .withClient(new ClientBuffer<>(SerializeUtils.DOUBLE, Double.class)).register();

        buildRegistry(ROTATIONAL_RESISTANCE)
                .withBasic(SerializePort.of(this::getRotationalResistance, this::setRotationalResistance, SerializeUtils.DOUBLE))
                .withClient(new ClientBuffer<>(SerializeUtils.DOUBLE, Double.class)).register();

        buildRegistry(RESISTANCE_AT_POS)
                .withBasic(SerializePort.of(this::isAirResistanceAtPos, this::setAirResistanceAtPos, SerializeUtils.BOOLEAN))
                .withClient(new ClientBuffer<>(SerializeUtils.BOOLEAN, Boolean.class)).register();

        buildRegistry(GRAVITY_AT_POS)
                .withBasic(SerializePort.of(this::isExtraGravityAtPos, this::setExtraGravityAtPos, SerializeUtils.BOOLEAN))
                .withClient(new ClientBuffer<>(SerializeUtils.BOOLEAN, Boolean.class)).register();

        buildRegistry(SQUARE_DRAG)
                .withBasic(SerializePort.of(this::squareDrag, this::setSquareDrag, SerializeUtils.BOOLEAN))
                .withClient(new ClientBuffer<>(SerializeUtils.BOOLEAN, Boolean.class)).register();

    }

    @Override
    public void tickServer() {
        syncAttachInducer();
    }

    public void syncAttachInducer(){
        if(level != null && level.isClientSide)return;
        Optional
            .ofNullable(getLoadedServerShip())
            .map(AnchorForceInducer::getOrCreate)
            .ifPresent(inducer -> inducer.replace(WorldBlockPos.of(level, getBlockPos()), this::getLogicalAnchor));
    }

    public void displayScreen(ServerPlayer player){
        var p = new BlockBoundClientPacket.builder(getBlockPos(), RegisteredPacketType.OPEN_SCREEN_0)
                .withDouble(airResistance)
                .withDouble(extraGravity)
                .withDouble(rotationalResistance)
                .build();

        ControlCraftPackets.sendToPlayer(p, player);
    }


    @Override
    public void handleServer(NetworkEvent.Context context, BlockBoundServerPacket packet) {
        if(packet.getType() == RegisteredPacketType.SETTING_0){
            airResistance = packet.getDoubles().get(0);
            extraGravity = packet.getDoubles().get(1);
            rotationalResistance = packet.getDoubles().get(2);
        }
    }

    public LogicalAnchor getLogicalAnchor() {
        return new LogicalAnchor(
                airResistance,
                extraGravity,
                rotationalResistance,
                WorldBlockPos.of(level, getBlockPos()),
                isAirResistanceAtPos(),
                isExtraGravityAtPos(),
                squareDrag()
        );
    }



}
