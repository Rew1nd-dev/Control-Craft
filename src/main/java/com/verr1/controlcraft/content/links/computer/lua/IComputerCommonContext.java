package com.verr1.controlcraft.content.links.computer.lua;

import com.verr1.controlcraft.foundation.cimulink.core.api.IPhysAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import net.minecraft.core.BlockPos;
import org.joml.Vector3dc;

public interface IComputerCommonContext {

    IPhysAccess getPhysAccess();

    IWorldAccess getWorldAccess();

    BlockPos getBlockPos();

    Vector3dc frontLocal();

    Vector3dc front();

    Vector3dc leftLocal();

    Vector3dc left();

    Vector3dc upLocal();

    Vector3dc up();

    Vector3dc positionModel();

}
