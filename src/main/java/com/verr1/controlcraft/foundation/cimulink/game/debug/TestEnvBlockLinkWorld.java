package com.verr1.controlcraft.foundation.cimulink.game.debug;

import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.core.BlockPos;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

public class TestEnvBlockLinkWorld {

    private final static HashMap<BlockPos, BlockLinkPort> ALL_PORTS = new HashMap<>();
    private static AtomicInteger count = new AtomicInteger(0);

    public static Optional<BlockLinkPort> get(WorldBlockPos pos){
        return Optional.ofNullable(ALL_PORTS.get(pos.pos()));
    }

    public static void add(BlockLinkPort... ports){
        Arrays.stream(ports).forEach(port -> {
            port.setWorldBlockPos(Debug.MapToDebug(count.getAndIncrement()));
            ALL_PORTS.put(port.pos().pos(), port);
        });
    }

}
