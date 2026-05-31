package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.content.links.computer.ComputerBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.network.NetworkEvent;

import java.util.Optional;

public class ComputerSyncPacket extends SimplePacketBase {
    private final BlockPos position;
    private final CompoundTag nbt;

    public ComputerSyncPacket(BlockPos position, CompoundTag nbt) {
        this.nbt = nbt;
        this.position = position;
    }

    public ComputerSyncPacket(FriendlyByteBuf buf) {
        this.nbt = buf.readNbt();
        this.position = buf.readBlockPos();
    }

    @Override
    public void write(FriendlyByteBuf buffer) {
        buffer.writeNbt(nbt);
        buffer.writeBlockPos(position);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() -> {
            Optional
                .ofNullable(Minecraft.getInstance().level)
                .flatMap(level -> BlockEntityGetter.getLevelBlockEntityAt(level, position, ComputerBlockEntity.class))
                .ifPresent(be -> {
                    be.receivePatchedData(nbt);
                });
        });
        return true;
    }
}