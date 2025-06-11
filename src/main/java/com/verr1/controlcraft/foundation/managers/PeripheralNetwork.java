package com.verr1.controlcraft.foundation.managers;


import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;


public class PeripheralNetwork {


    private final ConcurrentHashMap<WorldBlockPos, PeripheralKey> p2k = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<PeripheralKey, WorldBlockPos> k2p = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<WorldBlockPos, Integer> lives = new ConcurrentHashMap<>();

    static int TICKS_BEFORE_EXPIRED = 10;


    public void forceOnline(@NotNull PeripheralKey key, @NotNull WorldBlockPos pos){
        if(key.equals(PeripheralKey.NULL))return;
        offline(key);
        offline(pos);
        onlineUnchecked(key, pos);
        activate(pos);
    }

    private void offlineUnchecked(@NotNull PeripheralKey key, @NotNull WorldBlockPos pos){
        p2k.remove(pos);
        k2p.remove(key);
    }

    private void onlineUnchecked(@NotNull PeripheralKey key, @NotNull WorldBlockPos pos){
        p2k.put(pos, key);
        k2p.put(key, pos);
    }

    public void offline(@NotNull PeripheralKey key){
        Optional.ofNullable(k2p.get(key)).ifPresent(pos -> offlineUnchecked(key, pos));
    }

    public void offline(@NotNull WorldBlockPos pos){
        Optional.ofNullable(p2k.get(pos)).ifPresent(key -> offlineUnchecked(key, pos));
    }

    public void softOnline(@NotNull PeripheralKey key, @NotNull WorldBlockPos pos){

        // someone is using this key
        if(k2p.containsKey(key) && !k2p.get(key).equals(pos))return;


        forceOnline(key, pos);

    }

    public void activate(@NotNull WorldBlockPos pos){
        if(!p2k.containsKey(pos))return;
        lives.put(pos, TICKS_BEFORE_EXPIRED);
    }

    public void tick(){
        lives.forEach((k, v) -> {
            if(v < 0){
                offline(k);
            }
        });
        lives.entrySet().removeIf(e -> e.getValue() < 0);
        lives.entrySet().forEach(e-> e.setValue(e.getValue() - 1));
    }

    public boolean isRegistered(PeripheralKey key){
        return k2p.containsKey(key);
    }

    public boolean isRegistered(WorldBlockPos pos){
        return p2k.containsKey(pos);
    }


    public void free(PeripheralKey key){
        if(!isRegistered(key))return;
        WorldBlockPos pos = k2p.get(key);
        BlockEntityGetter.INSTANCE.getBlockEntityAt(pos.globalPos(), PeripheralInterfaceBlockEntity.class)
                .ifPresent(p -> {
                    p.setForced(false);
                    p.offline();
                });
    }

    public @NotNull PeripheralKey valid(WorldBlockPos pos){
        return p2k.getOrDefault(pos, PeripheralKey.NULL);
    }

    public @Nullable WorldBlockPos valid(PeripheralKey key) {
        return k2p.getOrDefault(key, null);
    }


    public record PeripheralKey(Long protocol, String name) {
        public static PeripheralKey NULL = new PeripheralKey(0L, "null");
        @Override
        public int hashCode() {
            return Long.hashCode(protocol);
        }

        public CompoundTag serialize(){
            return CompoundTagBuilder.create()
                    .withCompound("name", SerializeUtils.STRING.serialize(name))
                    .withCompound("protocol", SerializeUtils.LONG.serialize(protocol))
                    .build();
        }

        public static @NotNull PeripheralKey deserialize(@NotNull CompoundTag tag){
            if(!(tag.contains("name") && tag.contains("protocol")))return NULL;
            String name = SerializeUtils.STRING.deserialize(tag.getCompound("name"));
            long protocol = SerializeUtils.LONG.deserialize(tag.getCompound("protocol"));
            return new PeripheralKey(protocol, name);
        }
    }

}
