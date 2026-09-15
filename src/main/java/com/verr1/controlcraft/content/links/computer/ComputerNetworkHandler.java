package com.verr1.controlcraft.content.links.computer;

import com.verr1.controlcraft.content.links.computer.lua.LuaNbtSerializer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

public class ComputerNetworkHandler {

    public static final int MAX_EVENT_BACKLOG_PER_NAME = 32;

    private static final String TAG_FULL_SYNC = "full";
    private static final String TAG_STATES = "states";
    private static final String TAG_EVENTS = "events";
    private static final String TAG_REVISION = "revision";
    private static final String TAG_DELETED = "deleted";
    private static final String TAG_VALUE = "value";
    private static final String TAG_HAS_PAYLOAD = "hasPayload";
    private static final String TAG_NAME = "name";
    private static final String TAG_SEQ = "seq";
    private static final String TAG_TICK = "tick";
    private static final String TAG_SENDER = "sender";
    private static final String TAG_SIDE = "side";
    private static final String TAG_OWNER_UUID = "ownerUuid";

    private final Map<String, StateEntry> stateSlots = new HashMap<>();
    private final Map<String, Deque<EventEntry>> eventQueues = new HashMap<>();
    private final Map<String, StateSyncEntry> pendingStateSync = new LinkedHashMap<>();
    private final List<EventEntry> pendingOutboundEvents = new ArrayList<>();
    private final Map<String, Long> nextEventSeq = new HashMap<>();

    public boolean setState(String slot, Object value) {
        return setState(slot, value, false);
    }

    public boolean setState(String slot, Object value, boolean force) {
        Tag serializedValue = serializeNetworkValue(value);
        if (serializedValue == null) {
            return clearState(slot);
        }

        StateEntry entry = stateSlots.computeIfAbsent(slot, ignored -> new StateEntry());
        boolean changed = force || !entry.present || !Objects.equals(entry.serializedValue, serializedValue);
        if (!changed) {
            return false;
        }

        entry.value = value;
        entry.serializedValue = serializedValue;
        entry.present = true;
        entry.changed = true;
        entry.revision++;

        pendingStateSync.put(slot, StateSyncEntry.present(entry.revision, serializedValue));
        return true;
    }

    public boolean clearState(String slot) {
        StateEntry entry = stateSlots.get(slot);
        if (entry == null || !entry.present) {
            return false;
        }

        entry.value = null;
        entry.serializedValue = null;
        entry.present = false;
        entry.changed = true;
        entry.revision++;

        pendingStateSync.put(slot, StateSyncEntry.deleted(entry.revision));
        return true;
    }

    public boolean touchState(String slot) {
        StateEntry entry = stateSlots.get(slot);
        if (entry == null || !entry.present || entry.serializedValue == null) {
            return false;
        }

        entry.changed = true;
        entry.revision++;
        pendingStateSync.put(slot, StateSyncEntry.present(entry.revision, entry.serializedValue));
        return true;
    }

    public boolean isStateChanged(String slot) {
        StateEntry entry = stateSlots.get(slot);
        return entry != null && entry.changed;
    }

    public Object getState(String slot) {
        StateEntry entry = stateSlots.get(slot);
        if (entry == null || !entry.present) {
            return null;
        }
        return entry.value;
    }

    public Object consumeState(String slot) {
        StateEntry entry = stateSlots.get(slot);
        if (entry == null) {
            return null;
        }

        entry.changed = false;
        return entry.present ? entry.value : null;
    }

    public long getStateRevision(String slot) {
        StateEntry entry = stateSlots.get(slot);
        return entry == null ? 0L : entry.revision;
    }

    public Map<String, Object> getStates(Collection<String> keys) {
        Map<String, Object> values = new LinkedHashMap<>();
        for (String key : keys) {
            values.put(key, getState(key));
        }
        return values;
    }

    public EventEntry emitOutboundEvent(String name, @Nullable Object payload, long tick, EventSender sender) {
        long seq = nextEventSeq.merge(name, 1L, Long::sum);
        EventEntry entry = new EventEntry(name, seq, payload, tick, sender);
        pendingOutboundEvents.add(entry);
        return entry;
    }

    public void receiveInboundEvent(String name, long seq, @Nullable Object payload, long tick, EventSender sender) {
        Deque<EventEntry> queue = eventQueues.computeIfAbsent(name, ignored -> new ArrayDeque<>());
        while (queue.size() >= MAX_EVENT_BACKLOG_PER_NAME) {
            queue.pollFirst();
        }
        queue.offerLast(new EventEntry(name, seq, payload, tick, sender));
    }

    public @Nullable EventEntry peekEvent(String name) {
        Deque<EventEntry> queue = eventQueues.get(name);
        return queue == null ? null : queue.peekFirst();
    }

    public @Nullable EventEntry pollEvent(String name) {
        Deque<EventEntry> queue = eventQueues.get(name);
        return queue == null ? null : queue.pollFirst();
    }

    public int eventBacklog(String name) {
        Deque<EventEntry> queue = eventQueues.get(name);
        return queue == null ? 0 : queue.size();
    }

    public void clearEventQueue(String name) {
        Deque<EventEntry> queue = eventQueues.get(name);
        if (queue != null) {
            queue.clear();
        }
    }

    public long nextOutboundEventSeq(String name) {
        return nextEventSeq.getOrDefault(name, 0L) + 1L;
    }

    public boolean hasPendingSync() {
        return !pendingStateSync.isEmpty() || !pendingOutboundEvents.isEmpty();
    }

    public CompoundTag collectFullSyncTag() {
        CompoundTag root = new CompoundTag();
        root.putBoolean(TAG_FULL_SYNC, true);
        root.put(TAG_STATES, serializeAllPresentStates());
        return root;
    }

    public CompoundTag collectDeltaSyncTag() {
        CompoundTag root = new CompoundTag();
        root.putBoolean(TAG_FULL_SYNC, false);

        CompoundTag statesTag = new CompoundTag();
        pendingStateSync.forEach((key, value) -> statesTag.put(key, serializeStateSyncEntry(value)));
        root.put(TAG_STATES, statesTag);

        ListTag eventsTag = new ListTag();
        pendingOutboundEvents.forEach(event -> eventsTag.add(serializeEventEntry(event)));
        root.put(TAG_EVENTS, eventsTag);

        pendingStateSync.clear();
        pendingOutboundEvents.clear();
        return root;
    }

    public void applySyncTag(CompoundTag tag) {
        if (tag.getBoolean(TAG_FULL_SYNC)) {
            stateSlots.clear();
            eventQueues.clear();
            pendingStateSync.clear();
            pendingOutboundEvents.clear();
        }

        CompoundTag statesTag = tag.getCompound(TAG_STATES);
        for (String key : statesTag.getAllKeys()) {
            applyStateSync(key, statesTag.getCompound(key));
        }

        Tag eventsRaw = tag.get(TAG_EVENTS);
        if (eventsRaw instanceof ListTag eventsTag) {
            for (Tag element : eventsTag) {
                if (element instanceof CompoundTag eventTag) {
                    EventEntry event = deserializeEventEntry(eventTag);
                    receiveInboundEvent(event.name(), event.seq(), event.payload(), event.tick(), event.sender());
                }
            }
        }
    }

    private CompoundTag serializeAllPresentStates() {
        CompoundTag statesTag = new CompoundTag();
        stateSlots.forEach((key, entry) -> {
            if (!entry.present || entry.serializedValue == null) {
                return;
            }
            statesTag.put(key, serializeStateSyncEntry(StateSyncEntry.present(entry.revision, entry.serializedValue)));
        });
        return statesTag;
    }

    private void applyStateSync(String slot, CompoundTag syncTag) {
        StateEntry entry = stateSlots.computeIfAbsent(slot, ignored -> new StateEntry());
        entry.revision = syncTag.getLong(TAG_REVISION);
        entry.changed = true;

        if (syncTag.getBoolean(TAG_DELETED)) {
            entry.present = false;
            entry.value = null;
            entry.serializedValue = null;
            return;
        }

        Tag valueTag = syncTag.get(TAG_VALUE);
        if (valueTag == null) {
            entry.present = false;
            entry.value = null;
            entry.serializedValue = null;
            return;
        }

        entry.present = true;
        entry.serializedValue = valueTag.copy();
        entry.value = LuaNbtSerializer.deserialize(valueTag);
    }

    private CompoundTag serializeStateSyncEntry(StateSyncEntry entry) {
        CompoundTag tag = new CompoundTag();
        tag.putLong(TAG_REVISION, entry.revision);
        tag.putBoolean(TAG_DELETED, entry.deleted);
        if (!entry.deleted && entry.serializedValue != null) {
            tag.put(TAG_VALUE, entry.serializedValue.copy());
        }
        return tag;
    }

    private CompoundTag serializeEventEntry(EventEntry event) {
        CompoundTag tag = new CompoundTag();
        tag.putString(TAG_NAME, event.name());
        tag.putLong(TAG_SEQ, event.seq());
        tag.putLong(TAG_TICK, event.tick());
        tag.put(TAG_SENDER, serializeEventSender(event.sender()));

        Tag payloadTag = serializeEventPayload(event.payload());
        if (payloadTag != null) {
            tag.putBoolean(TAG_HAS_PAYLOAD, true);
            tag.put(TAG_VALUE, payloadTag);
        } else {
            tag.putBoolean(TAG_HAS_PAYLOAD, false);
        }
        return tag;
    }

    private EventEntry deserializeEventEntry(CompoundTag tag) {
        Object payload = null;
        if (tag.getBoolean(TAG_HAS_PAYLOAD)) {
            Tag payloadTag = tag.get(TAG_VALUE);
            payload = payloadTag == null ? null : LuaNbtSerializer.deserialize(payloadTag);
        }

        EventSender sender = deserializeEventSender(tag.getCompound(TAG_SENDER));
        return new EventEntry(
                tag.getString(TAG_NAME),
                tag.getLong(TAG_SEQ),
                payload,
                tag.getLong(TAG_TICK),
                sender
        );
    }

    private CompoundTag serializeEventSender(EventSender sender) {
        CompoundTag tag = new CompoundTag();
        tag.putString(TAG_SIDE, sender.side());
        if (sender.ownerUuid() != null) {
            tag.putUUID(TAG_OWNER_UUID, sender.ownerUuid());
        }
        return tag;
    }

    private EventSender deserializeEventSender(CompoundTag tag) {
        UUID ownerUuid = tag.hasUUID(TAG_OWNER_UUID) ? tag.getUUID(TAG_OWNER_UUID) : null;
        return new EventSender(tag.getString(TAG_SIDE), ownerUuid);
    }

    private static Tag serializeNetworkValue(Object value) {
        Tag tag = LuaNbtSerializer.serialize(value);
        if (tag == null || tag.getId() == Tag.TAG_END) {
            return null;
        }
        return tag.copy();
    }

    private static Tag serializeEventPayload(@Nullable Object payload) {
        if (payload == null) {
            return null;
        }
        Tag tag = LuaNbtSerializer.serialize(payload);
        if (tag == null || tag.getId() == Tag.TAG_END) {
            return null;
        }
        return tag.copy();
    }

    private static final class StateEntry {
        private @Nullable Object value;
        private @Nullable Tag serializedValue;
        private long revision;
        private boolean changed;
        private boolean present;
    }

    private record StateSyncEntry(long revision, boolean deleted, @Nullable Tag serializedValue) {
        private static StateSyncEntry present(long revision, @Nullable Tag serializedValue) {
            return new StateSyncEntry(revision, false, serializedValue == null ? null : serializedValue.copy());
        }

        private static StateSyncEntry deleted(long revision) {
            return new StateSyncEntry(revision, true, null);
        }
    }

    public record EventEntry(
            String name,
            long seq,
            @Nullable Object payload,
            long tick,
            EventSender sender
    ) {
    }

    public record EventSender(String side, @Nullable UUID ownerUuid) {
        public static EventSender server() {
            return new EventSender("server", null);
        }

        public static EventSender client(@Nullable UUID ownerUuid) {
            return new EventSender("client", ownerUuid);
        }
    }
}
