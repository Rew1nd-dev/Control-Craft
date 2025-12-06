package com.verr1.controlcraft.content.compact.shaolib;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.compact.tweak.ITweakedControllerComponentGetter;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.ClipContext;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.fml.ModList;

public class ShaoLibCompact {

    public static IShaoLibProxy getter;


    public static BlockHitResult fastClip(ServerLevel level, Vec3 start, Vec3 end) {
        if(getter == null)return level.clip(new ClipContext(start, end, ClipContext.Block.COLLIDER, ClipContext.Fluid.NONE, null));
        return getter.fastClip(level, start, end);
    }

    public static void init(){
        if(!ModList.get().isLoaded("shaolib"))return;
        try {
            Class<?> clazz = Class.forName("com.verr1.controlcraft.content.compact.shaolib.impl.ShaoLibProxyImpl");
            getter = (IShaoLibProxy) clazz.getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            ControlCraft.LOGGER.error("Failed to initialize ShaoLib Compact", e);
        }
    }

}
