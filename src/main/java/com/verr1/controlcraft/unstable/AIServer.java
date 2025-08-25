package com.verr1.controlcraft.unstable;

import com.verr1.controlcraft.unstable.data.schematic.AIServerSchematics;
import com.verr1.controlcraft.unstable.management.AIPool;
import net.minecraft.server.MinecraftServer;

public class AIServer {

    public static final AIServerSchematics SCHEMATICS_MANAGER = new AIServerSchematics();
    public static AIPool MANAGER;


    public static void init(MinecraftServer server){
        SCHEMATICS_MANAGER.loadSchematics(server);
        MANAGER = AIPool.load(server);
    }
}
