package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.content.links.computer.ComputerBlockEntity;
import com.verr1.controlcraft.content.links.computer.lua.LuaNbtSerializer;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.EndTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

public class ComputerClientNetworkPacket extends SimplePacketBase {

    private static final int MAX_SINGLE_PAYLOAD_BYTES = 1024;

    private final BlockPos position;
    private final String slot;
    private final CompoundTag payload;

    public ComputerClientNetworkPacket(BlockPos position, String slot, Object value) {
        this.position = position;
        this.slot = slot;
        this.payload = new CompoundTag();

        Tag valueTag = LuaNbtSerializer.serialize(value);
        if (valueTag == null || valueTag instanceof EndTag) {
            payload.putBoolean("_nil", true);
        } else {
            payload.put("v", valueTag);
        }
    }

    public ComputerClientNetworkPacket(FriendlyByteBuf buf) {
        this.position = buf.readBlockPos();
        this.slot = buf.readUtf();
        CompoundTag nbt = buf.readNbt();
        this.payload = nbt == null ? new CompoundTag() : nbt;
    }

    @Override
    public void write(FriendlyByteBuf buffer) {
        buffer.writeBlockPos(position);
        buffer.writeUtf(slot);
        buffer.writeNbt(payload);
    }

    @Override
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() -> {
            ServerPlayer sender = context.getSender();
            if (sender == null) {
                return;
            }

            int incomingBytes = payload.sizeInBytes() + slot.length() * 2;
            if (incomingBytes > MAX_SINGLE_PAYLOAD_BYTES) {
                return;
            }

            BlockEntityGetter.getLevelBlockEntityAt(sender.serverLevel(), position, ComputerBlockEntity.class)
                    .ifPresent(be -> {
                        if (payload.getBoolean("_nil")) {
                            be.getNetworkHandler().set(slot, null);
                            return;
                        }

                        Tag valueTag = payload.get("v");
                        if (valueTag == null) {
                            return;
                        }
                        Object value = LuaNbtSerializer.deserialize(valueTag);
                        be.getNetworkHandler().set(slot, value);
                    });
        });
        return true;
    }
}
