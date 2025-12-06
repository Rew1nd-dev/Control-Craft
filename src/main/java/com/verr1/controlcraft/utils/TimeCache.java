package com.verr1.controlcraft.utils;

public class TimeCache {

    private static long cachedTime = System.currentTimeMillis();

    // called by 2 thread when playing single player, may cause competition, but who cares
    public static void tick(){
        cachedTime = System.currentTimeMillis();
    }

    public static long time(){
        return cachedTime;
    }

}
