package com.verr1.controlcraft.foundation.cimulink.game.debug;

import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.core.BlockPos;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Optional;

public class TestEnvBlockLinkWorld {

    private final static HashMap<BlockPos, BlockLinkPort> ALL_PORTS = new HashMap<>();


    public static Optional<BlockLinkPort> get(WorldBlockPos pos){
        return Optional.ofNullable(ALL_PORTS.get(pos.pos()));
    }

    public static void add(BlockLinkPort... ports){
        Arrays.stream(ports).forEach(port -> ALL_PORTS.put(port.pos().pos(), port));
    }

}
