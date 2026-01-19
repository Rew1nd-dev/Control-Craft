package com.verr1.controlcraft.content.compact.shaolib.impl;

import com.verr1.controlcraft.content.compact.shaolib.IShaoLibProxy;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;
import net.shao.shaolib.api.LodRaycasts;

public class ShaoLibProxyImpl implements IShaoLibProxy {


    public ShaoLibProxyImpl() {}

    @Override
    public BlockHitResult fastClip(ServerLevel level, Vec3 start, Vec3 end) {
        return LodRaycasts.clipActualOrCachedIncludeShips(level, start, end, true, false, $ -> true);
    }
}
