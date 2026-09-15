package com.verr1.controlcraft.foundation.network.packets.specific;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.controlcraft.content.links.computer.ComputerBlockEntity;
import com.verr1.controlcraft.content.links.computer.ComputerNetworkHandler;
import com.verr1.controlcraft.content.links.computer.lua.LuaNbtSerializer;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;
import org.jetbrains.annotations.Nullable;

public class ComputerClientNetworkPacket extends SimplePacketBase {

    public static final int MAX_SINGLE_PAYLOAD_BYTES = 5 * 1024;

    private static final String TAG_HAS_VALUE = "hasValue";
    private static final String TAG_VALUE = "value";

    private final BlockPos position;
    private final Operation operation;
    private final String key;
    private final CompoundTag payload;
    private final long authorityToken;
    private final long eventSeq;
    private final long eventTick;

    private ComputerClientNetworkPacket(
            BlockPos position,
            Operation operation,
            String key,
            CompoundTag payload,
            long authorityToken,
            long eventSeq,
            long eventTick
    ) {
        this.position = position;
        this.operation = operation;
        this.key = key;
        this.payload = payload == null ? new CompoundTag() : payload;
        this.authorityToken = authorityToken;
        this.eventSeq = eventSeq;
        this.eventTick = eventTick;
    }

    public static ComputerClientNetworkPacket stateSet(BlockPos position, String slot, Object value, long authorityToken) {
        CompoundTag payload = new CompoundTag();
        Tag valueTag = LuaNbtSerializer.serialize(value);
        if (valueTag != null && valueTag.getId() != Tag.TAG_END) {
            payload.put(TAG_VALUE, valueTag);
        }
        return new ComputerClientNetworkPacket(position, Operation.STATE_SET, slot, payload, authorityToken, 0L, 0L);
    }

    public static ComputerClientNetworkPacket stateClear(BlockPos position, String slot, long authorityToken) {
        return new ComputerClientNetworkPacket(position, Operation.STATE_CLEAR, slot, new CompoundTag(), authorityToken, 0L, 0L);
    }

    public static ComputerClientNetworkPacket eventEmit(
            BlockPos position,
            String name,
            @Nullable Object payloadValue,
            long seq,
            long tick,
            long authorityToken
    ) {
        CompoundTag payload = new CompoundTag();
        if (payloadValue != null) {
            Tag valueTag = LuaNbtSerializer.serialize(payloadValue);
            if (valueTag != null && valueTag.getId() != Tag.TAG_END) {
                payload.putBoolean(TAG_HAS_VALUE, true);
                payload.put(TAG_VALUE, valueTag);
            }
        } else {
            payload.putBoolean(TAG_HAS_VALUE, false);
        }
        if (!payload.contains(TAG_HAS_VALUE)) {
            payload.putBoolean(TAG_HAS_VALUE, false);
        }
        return new ComputerClientNetworkPacket(position, Operation.EVENT_EMIT, name, payload, authorityToken, seq, tick);
    }

    public ComputerClientNetworkPacket(FriendlyByteBuf buf) {
        this.position = buf.readBlockPos();
        this.operation = buf.readEnum(Operation.class);
        this.key = buf.readUtf();
        this.authorityToken = buf.readLong();
        this.eventSeq = buf.readLong();
        this.eventTick = buf.readLong();
        CompoundTag nbt = buf.readNbt();
        this.payload = nbt == null ? new CompoundTag() : nbt;
    }

    public int payloadBytes() {
        return payload.sizeInBytes() + key.length() * 2 + 32;
    }

    @Override
    public void write(FriendlyByteBuf buffer) {
        buffer.writeBlockPos(position);
        buffer.writeEnum(operation);
        buffer.writeUtf(key);
        buffer.writeLong(authorityToken);
        buffer.writeLong(eventSeq);
        buffer.writeLong(eventTick);
        buffer.writeNbt(payload);
    }

    @Override
    public boolean handle(NetworkEvent.Context context) {
        context.enqueueWork(() -> {
            ServerPlayer sender = context.getSender();
            if (sender == null) {
                return;
            }

            if (payloadBytes() > MAX_SINGLE_PAYLOAD_BYTES) {
                return;
            }

            BlockEntityGetter.getLevelBlockEntityAt(sender.serverLevel(), position, ComputerBlockEntity.class)
                    .ifPresent(be -> {
                        if (!be.acceptClientNetworkInput(sender, authorityToken)) {
                            return;
                        }

                        switch (operation) {
                            case STATE_SET -> handleStateSet(be);
                            case STATE_CLEAR -> be.getNetworkHandler().clearState(key);
                            case EVENT_EMIT -> handleEventEmit(be, sender);
                        }
                    });
        });
        return true;
    }

    private void handleStateSet(ComputerBlockEntity be) {
        Tag valueTag = payload.get(TAG_VALUE);
        if (valueTag == null) {
            return;
        }
        Object value = LuaNbtSerializer.deserialize(valueTag);
        be.getNetworkHandler().setState(key, value);
    }

    private void handleEventEmit(ComputerBlockEntity be, ServerPlayer sender) {
        Object payloadValue = null;
        if (payload.getBoolean(TAG_HAS_VALUE)) {
            Tag valueTag = payload.get(TAG_VALUE);
            payloadValue = valueTag == null ? null : LuaNbtSerializer.deserialize(valueTag);
        }

        be.getNetworkHandler().receiveInboundEvent(
                key,
                eventSeq,
                payloadValue,
                eventTick,
                ComputerNetworkHandler.EventSender.client(sender.getUUID())
        );
    }

    private enum Operation {
        STATE_SET,
        STATE_CLEAR,
        EVENT_EMIT
    }
}
