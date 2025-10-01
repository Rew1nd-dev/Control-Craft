package com.verr1.controlcraft.content.compact.shaolib;

import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;

public interface IShaoLibProxy {

    BlockHitResult fastClip(ServerLevel level, Vec3 start, Vec3 end);

}
