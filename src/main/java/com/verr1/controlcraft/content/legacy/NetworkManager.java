package com.verr1.controlcraft.content.legacy;

import net.minecraft.core.BlockPos;
import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class NetworkManager {
    static Map<PeripheralKey_, BlockPos> REGISTERED_POS = new ConcurrentHashMap<>();
    static private final Map<BlockPos, PeripheralKey_> REGISTERED_KEYS = new ConcurrentHashMap<>();

    static Map<BlockPos, Integer> KEY_LIVES = new ConcurrentHashMap<>(); //dealing with unknown miss-unregister issue
    static int TICKS_BEFORE_EXPIRED = 10;

    public static void UnregisterWirelessPeripheral(PeripheralKey_ key) {
        if(key == null)return;
        if(!isRegistered(key))return;
        BlockPos value = REGISTERED_POS.get(key);
        unregister(key, value);
    }

    private static void unregister(PeripheralKey_ key, BlockPos value) {
        REGISTERED_POS.remove(key);
        REGISTERED_KEYS.remove(value);
        KEY_LIVES.remove(value);
    }

    private static void register(PeripheralKey_ key, BlockPos pos) {
        REGISTERED_POS.put(key, pos);
        REGISTERED_KEYS.put(pos, key);
        KEY_LIVES.put(pos, TICKS_BEFORE_EXPIRED);
    }

    public static void activate(BlockPos pos){
        if(!isRegistered(pos))return;
        KEY_LIVES.put(pos, TICKS_BEFORE_EXPIRED);
    }

    private static void tickActivated(){
        KEY_LIVES.forEach((k, v) -> {
            if(v < 0){
                UnregisterWirelessPeripheral(REGISTERED_KEYS.get(k));
            }
        });
        KEY_LIVES.entrySet().removeIf(e -> e.getValue() < 0);
        KEY_LIVES.entrySet().forEach(e-> e.setValue(e.getValue() - 1));
    }

    public static void tick(){
        tickActivated();
    }

    private static boolean canMove(PeripheralKey_ newKey, BlockPos content){
        return isRegistered(content) && !isRegistered(newKey);
    }


    private static void move(PeripheralKey_ newKey, BlockPos content){
        if(!canMove(newKey, content))return;
        PeripheralKey_ oldKey = REGISTERED_KEYS.get(content);
        REGISTERED_POS.remove(oldKey);
        REGISTERED_KEYS.remove(content);
        register(newKey, content);
    }

    public static boolean isRegistered(BlockPos pos){
        return REGISTERED_KEYS.containsKey(pos);
    }

    public static boolean isRegistered(PeripheralKey_ key){
        return REGISTERED_POS.containsKey(key);
    }

    public static BlockPos getRegisteredPeripheralPos(PeripheralKey_ key) {
        if(!isRegistered(key))return null;
        return REGISTERED_POS.get(key);
    }


    public static PeripheralKey_ registerAndGetKey(@NotNull PeripheralKey_ newKey, BlockPos content) {
        if(isRegistered(content)){
            if(canMove(newKey, content)){
                move(newKey, content);
                return newKey;
            }
            return REGISTERED_KEYS.get(content);
        }
        if(isRegistered(newKey))return PeripheralKey_.NULL;
        register(newKey, content);

        return newKey;
    }

}
