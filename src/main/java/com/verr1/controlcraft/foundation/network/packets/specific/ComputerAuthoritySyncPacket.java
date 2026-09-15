package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.content.links.computer.ComputerAuthorityMode;
import com.verr1.controlcraft.content.links.computer.ComputerAuthoritySnapshot;
import com.verr1.controlcraft.content.links.computer.ComputerBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.network.NetworkEvent;
import org.jetbrains.annotations.Nullable;

import java.util.Optional;
import java.util.UUID;

public class ComputerAuthoritySyncPacket extends SimplePacketBase {

    private final BlockPos position;
    private final ComputerAuthoritySnapshot snapshot;

    public ComputerAuthoritySyncPacket(BlockPos position, ComputerAuthoritySnapshot snapshot) {
        this.position = position;
        this.snapshot = snapshot == null ? ComputerAuthoritySnapshot.none() : snapshot;
    }

    public ComputerAuthoritySyncPacket(FriendlyByteBuf buf) {
        this.position = buf.readBlockPos();
        ComputerAuthorityMode mode = buf.readEnum(ComputerAuthorityMode.class);
        UUID ownerId = buf.readBoolean() ? buf.readUUID() : null;
        long leaseExpireTick = buf.readLong();
        long token = buf.readLong();
        this.snapshot = new ComputerAuthoritySnapshot(mode, ownerId, leaseExpireTick, token);
    }

    @Override
    public void write(FriendlyByteBuf buffer) {
        buffer.writeBlockPos(position);
        buffer.writeEnum(snapshot.mode());
        buffer.writeBoolean(snapshot.ownerId() != null);
        if (snapshot.ownerId() != null) {
            buffer.writeUUID(snapshot.ownerId());
        }
        buffer.writeLong(snapshot.leaseExpireTick());
        buffer.writeLong(snapshot.token());
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() -> {
            Optional.ofNullable(Minecraft.getInstance().level)
                    .flatMap(level -> BlockEntityGetter.getLevelBlockEntityAt(level, position, ComputerBlockEntity.class))
                    .ifPresent(be -> be.receiveAuthoritySync(snapshot));
        });
        return true;
    }
}
