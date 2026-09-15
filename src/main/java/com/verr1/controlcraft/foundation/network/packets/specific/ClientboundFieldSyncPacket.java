package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.content.blocks.NetworkBlockEntity;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.network.NetworkEvent;

import java.util.Optional;

public class ClientboundFieldSyncPacket extends SimplePacketBase {
    private final BlockPos pos;
    private final byte[] payload;

    public ClientboundFieldSyncPacket(BlockPos pos, byte[] payload) {
        this.pos = pos;
        this.payload = payload;
    }

    public ClientboundFieldSyncPacket(FriendlyByteBuf buf) {
        payload = buf.readByteArray();
        pos = buf.readBlockPos();
    }

    @Override
    public void write(FriendlyByteBuf buffer) {
        buffer.writeByteArray(payload);
        buffer.writeBlockPos(pos);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() ->
                Optional.ofNullable(Minecraft.getInstance().level)
                        .map(level -> level.getExistingBlockEntity(pos))
                        .filter(NetworkBlockEntity.class::isInstance)
                        .map(NetworkBlockEntity.class::cast)
                        .ifPresent(be -> be.clientboundFieldSync().receiveSync(payload)));
        return true;
    }
}
