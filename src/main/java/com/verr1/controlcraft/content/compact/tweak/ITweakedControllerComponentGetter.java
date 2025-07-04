package com.verr1.controlcraft.content.compact.tweak;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.state.BlockState;

public interface ITweakedControllerComponentGetter {

    NamedComponent tweakedControllerPlant(ServerLevel level, BlockPos pos);

    BlockState lecternBlock();

}
