package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.content.links.computer.ComputerBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

public class ComputerAuthorityReleasePacket extends SimplePacketBase {

    private final BlockPos position;

    public ComputerAuthorityReleasePacket(BlockPos position) {
        this.position = position;
    }

    public ComputerAuthorityReleasePacket(FriendlyByteBuf buf) {
        this.position = buf.readBlockPos();
    }

    @Override
    public void write(FriendlyByteBuf buffer) {
        buffer.writeBlockPos(position);
    }

    @Override
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() -> {
            ServerPlayer sender = context.getSender();
            if (sender == null) {
                return;
            }

            BlockEntityGetter.getLevelBlockEntityAt(sender.serverLevel(), position, ComputerBlockEntity.class)
                    .ifPresent(be -> be.releaseNetworkAuthority(sender));
        });
        return true;
    }
}
