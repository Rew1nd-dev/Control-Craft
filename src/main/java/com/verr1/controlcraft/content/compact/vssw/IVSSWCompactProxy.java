package com.verr1.controlcraft.content.compact.vssw;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;

public interface IVSSWCompactProxy {

    NamedComponent getVSSWSeatPlant(ServerLevel level, BlockPos pos);

}
