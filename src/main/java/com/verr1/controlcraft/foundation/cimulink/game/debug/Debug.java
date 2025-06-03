package com.verr1.controlcraft.foundation.cimulink.game.debug;

import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.core.BlockPos;

public class Debug {

    public static boolean TEST_ENVIRONMENT = true;


    public static WorldBlockPos MapToDebug(int id){
        return new WorldBlockPos("debug_level", BlockPos.of(id));
    }

    public static long toID(BlockLinkPort port){
        return port.pos().pos().asLong();
    }

}
