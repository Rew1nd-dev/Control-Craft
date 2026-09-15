package com.verr1.controlcraft.foundation.network.sync;

import com.simibubi.create.foundation.blockEntity.SmartBlockEntity;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.packets.specific.ClientboundFieldSyncPacket;
import com.verr1.controlcraft.foundation.network.packets.specific.RequestClientboundFieldSyncPacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import io.netty.buffer.Unpooled;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.PacketDistributor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class ClientboundFieldSyncHandler {
    private static final int AUTO_SYNC_INTERVAL = 20 * 5;

    private final SmartBlockEntity delegate;
    private final Map<NetworkKey, Entry<?>> entriesByKey = new LinkedHashMap<>();
    private final Map<Integer, Entry<?>> entriesById = new HashMap<>();
    private final AtomicReference<Set<NetworkKey>> queuedKeys =
            new AtomicReference<>(ConcurrentHashMap.newKeySet());

    private int autoSyncCooldown = AUTO_SYNC_INTERVAL;

    public ClientboundFieldSyncHandler(SmartBlockEntity delegate) {
        this.delegate = delegate;
    }

    public <T> Registry<T> buildRegistry(NetworkKey key, BufCodec<T> codec) {
        return new Registry<>(key, codec);
    }

    public void initializeServer() {
        if (entriesByKey.isEmpty() || delegate.getLevel() == null || delegate.getLevel().isClientSide) {
            return;
        }

        syncForNear(entriesByKey.keySet());
    }

    public void tickServer() {
        if (entriesByKey.isEmpty() || delegate.getLevel() == null || delegate.getLevel().isClientSide) {
            return;
        }

        if (--autoSyncCooldown <= 0) {
            autoSyncCooldown = AUTO_SYNC_INTERVAL;
            queueAll();
        }

        flushQueued();
    }

    public void queue(NetworkKey... keys) {
        if (delegate.getLevel() != null && delegate.getLevel().isClientSide) {
            return;
        }

        Set<NetworkKey> pending = queuedKeys.get();
        for (NetworkKey key : keys) {
            if (entriesByKey.containsKey(key)) {
                pending.add(key);
            }
        }
    }

    public void queueAll() {
        if (entriesByKey.isEmpty()) {
            return;
        }

        queue(entriesByKey.keySet().toArray(NetworkKey[]::new));
    }

    public void request(NetworkKey... keys) {
        if (delegate.getLevel() == null || !delegate.getLevel().isClientSide) {
            return;
        }

        List<NetworkKey> requested = resolveRequestedKeys(keys);
        if (requested.isEmpty()) {
            return;
        }

        ControlCraftPackets.getChannel().sendToServer(
                new RequestClientboundFieldSyncPacket(delegate.getBlockPos(), requested)
        );
    }

    public void receiveRequest(List<NetworkKey> keys, ServerPlayer player) {
        if (delegate.getLevel() == null || delegate.getLevel().isClientSide) {
            return;
        }

        List<Entry<?>> entries = resolveEntries(keys);
        if (entries.isEmpty()) {
            return;
        }

        dispatch(PacketDistributor.PLAYER.with(() -> player), entries);
    }

    public void receiveSync(byte[] payload) {
        FriendlyByteBuf buffer = new FriendlyByteBuf(Unpooled.wrappedBuffer(payload));
        try {
            int size = buffer.readVarInt();
            for (int i = 0; i < size && buffer.isReadable(); i++) {
                int fieldId = buffer.readVarInt();
                int fieldSize = buffer.readVarInt();
                if (!buffer.isReadable(fieldSize)) {
                    break;
                }

                FriendlyByteBuf fieldBuffer = new FriendlyByteBuf(buffer.readSlice(fieldSize));
                Entry<?> entry = entriesById.get(fieldId);
                if (entry == null) {
                    continue;
                }

                try {
                    entry.apply(fieldBuffer);
                } catch (Exception e) {
                    ControlCraft.LOGGER.warn(
                            "Failed to apply synced field {} for {} at {}: {}",
                            entry.key.getSerializedName(),
                            delegate.getClass().getSimpleName(),
                            delegate.getBlockPos(),
                            e.getMessage()
                    );
                }
            }
        } finally {
            buffer.release();
        }
    }

    private void flushQueued() {
        Set<NetworkKey> pending = queuedKeys.getAndSet(ConcurrentHashMap.newKeySet());
        if (pending.isEmpty()) {
            return;
        }

        List<Entry<?>> entries = resolveEntries(pending);
        if (entries.isEmpty()) {
            return;
        }

        dispatch(PacketDistributor.ALL.noArg(), entries);
    }

    private void syncForNear(Collection<NetworkKey> keys) {
        if (delegate.getLevel() == null) {
            return;
        }

        List<Entry<?>> entries = resolveEntries(keys);
        if (entries.isEmpty()) {
            return;
        }

        BlockPos pos = delegate.getBlockPos();
        dispatch(
                PacketDistributor.NEAR.with(
                        () -> new PacketDistributor.TargetPoint(
                                pos.getX(),
                                pos.getY(),
                                pos.getZ(),
                                64,
                                delegate.getLevel().dimension()
                        )
                ),
                entries
        );
    }

    private void dispatch(PacketDistributor.PacketTarget target, List<Entry<?>> entries) {
        if (delegate.getLevel() == null || delegate.getLevel().isClientSide || entries.isEmpty()) {
            return;
        }

        ControlCraftPackets.getChannel().send(
                target,
                new ClientboundFieldSyncPacket(delegate.getBlockPos(), encode(entries))
        );
    }

    private byte[] encode(List<Entry<?>> entries) {
        FriendlyByteBuf buffer = new FriendlyByteBuf(Unpooled.buffer());
        try {
            buffer.writeVarInt(entries.size());
            for (Entry<?> entry : entries) {
                FriendlyByteBuf fieldBuffer = new FriendlyByteBuf(Unpooled.buffer());
                try {
                    entry.encode(fieldBuffer);
                    buffer.writeVarInt(entry.id);
                    buffer.writeVarInt(fieldBuffer.readableBytes());
                    buffer.writeBytes(fieldBuffer, fieldBuffer.readerIndex(), fieldBuffer.readableBytes());
                } catch (Exception e) {
                    ControlCraft.LOGGER.warn(
                            "Failed to encode synced field {} for {} at {}: {}",
                            entry.key.getSerializedName(),
                            delegate.getClass().getSimpleName(),
                            delegate.getBlockPos(),
                            e.getMessage()
                    );
                } finally {
                    fieldBuffer.release();
                }
            }

            byte[] payload = new byte[buffer.readableBytes()];
            buffer.getBytes(buffer.readerIndex(), payload);
            return payload;
        } finally {
            buffer.release();
        }
    }

    private List<NetworkKey> resolveRequestedKeys(NetworkKey... keys) {
        if (keys == null || keys.length == 0) {
            return new ArrayList<>(entriesByKey.keySet());
        }

        List<NetworkKey> requested = new ArrayList<>();
        for (NetworkKey key : keys) {
            if (entriesByKey.containsKey(key)) {
                requested.add(key);
            }
        }
        return requested;
    }


    private List<Entry<?>> resolveEntries(Collection<NetworkKey> keys) {
        Collection<NetworkKey> requestedKeys = keys == null || keys.isEmpty() ? entriesByKey.keySet() : keys;
        List<Entry<?>> entries = new ArrayList<>();
        for (NetworkKey key : requestedKeys) {
            Entry<?> entry = entriesByKey.get(key);
            if (entry != null) {
                entries.add(entry);
            }
        }
        entries.sort(Comparator.comparingInt(entry -> entry.id));
        return entries;
    }

    private <T> void register(Registry<T> registry) {
        if (entriesByKey.containsKey(registry.key)) {
            throw new IllegalStateException("Duplicate clientbound field key: " + registry.key.getSerializedName());
        }

        int id = entriesByKey.size();
        Entry<T> entry = new Entry<>(
                id,
                registry.key,
                registry.codec,
                Objects.requireNonNull(registry.serverGetter, "Server getter is required."),
                registry.clientApplier == null ? value -> { } : registry.clientApplier
        );
        entriesByKey.put(registry.key, entry);
        entriesById.put(id, entry);
    }

    private static class Entry<T> {
        private final int id;
        private final NetworkKey key;
        private final BufCodec<T> codec;
        private final Supplier<T> serverGetter;
        private final Consumer<T> clientApplier;

        private Entry(int id, NetworkKey key, BufCodec<T> codec, Supplier<T> serverGetter, Consumer<T> clientApplier) {
            this.id = id;
            this.key = key;
            this.codec = codec;
            this.serverGetter = serverGetter;
            this.clientApplier = clientApplier;
        }

        private void encode(FriendlyByteBuf buffer) {
            codec.encode(buffer, serverGetter.get());
        }

        private void apply(FriendlyByteBuf buffer) {
            clientApplier.accept(codec.decode(buffer));
        }
    }

    public class Registry<T> {
        private final NetworkKey key;
        private final BufCodec<T> codec;
        private Supplier<T> serverGetter;
        private Consumer<T> clientApplier;

        private Registry(NetworkKey key, BufCodec<T> codec) {
            this.key = key;
            this.codec = codec;
        }

        public Registry<T> withServerGetter(Supplier<T> serverGetter) {
            this.serverGetter = serverGetter;
            return this;
        }

        public Registry<T> withClientApplier(Consumer<T> clientApplier) {
            this.clientApplier = clientApplier;
            return this;
        }

        public void register() {
            ClientboundFieldSyncHandler.this.register(this);
        }
    }
}
