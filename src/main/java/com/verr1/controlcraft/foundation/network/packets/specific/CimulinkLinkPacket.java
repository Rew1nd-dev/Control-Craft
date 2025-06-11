package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.links.BlockPort;
import com.verr1.controlcraft.foundation.data.links.ClientViewContext;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraftforge.network.NetworkEvent;

public class CimulinkLinkPacket extends SimplePacketBase {

    private final BlockPort inputPort;
    private final BlockPort outputPort;

    public CimulinkLinkPacket(BlockPort inputPort, BlockPort outputPort) {
        this.inputPort = inputPort;
        this.outputPort = outputPort;
    }

    public CimulinkLinkPacket(FriendlyByteBuf buffer) {
        this.inputPort = new BlockPort(
                new WorldBlockPos(buffer.readUtf(), buffer.readBlockPos()),
                buffer.readUtf()
        );
        this.outputPort = new BlockPort(
                new WorldBlockPos(buffer.readUtf(), buffer.readBlockPos()),
                buffer.readUtf()
        );
    }

    @Override
    public void write(FriendlyByteBuf buffer) {

        buffer.writeUtf(inputPort.pos().dimensionID());
        buffer.writeBlockPos(inputPort.pos().pos());
        buffer.writeUtf(inputPort.portName());

        buffer.writeUtf(outputPort.pos().dimensionID());
        buffer.writeBlockPos(outputPort.pos().pos());
        buffer.writeUtf(outputPort.portName());
    }

    @Override
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() -> {
            try{

                BlockLinkPort.of(outputPort.pos()).ifPresent(
                        blp -> {
                            if(outputPort.pos().pos().getCenter().distanceToSqr(inputPort.pos().pos().getCenter()) > 1e4){
                                throw new IllegalArgumentException("Ports Are Too Faraway From Each Other");
                            }
                            blp.connectTo(
                                    outputPort.portName(),
                                    inputPort.pos(),
                                    inputPort.portName()
                            );
                        }

                );
                BlockEntityGetter.INSTANCE
                        .getBlockEntityAt(outputPort.pos(), CimulinkBlockEntity.class)
                        .ifPresent(BlockEntity::setChanged);
            }catch (IllegalArgumentException e){
                if(context.getSender() == null)return;
                context.getSender().sendSystemMessage(Component.literal(e.getMessage()));
            }
        });
        return true;
    }
}
