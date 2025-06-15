package com.verr1.controlcraft.content.compact.tweak;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraftforge.fml.ModList;
import org.jetbrains.annotations.Nullable;

public class TweakControllerCompact {

    public static ITweakedControllerComponentGetter getter;

    public static @Nullable NamedComponent tweakedControllerPlant(ServerLevel level, BlockPos pos){
        if(getter == null)return null;
        return getter.tweakedControllerPlant(level, pos);
    }

    public static void init(){
        if(!ModList.get().isLoaded("create_tweaked_controllers"))return;
        // get constructor using reflect
        try {
            Class<?> clazz = Class.forName("com.verr1.controlcraft.content.compact.tweak.impl.ITweakedControllerComponentGetterImpl");
            getter = (ITweakedControllerComponentGetter) clazz.getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            ControlCraft.LOGGER.error("Failed to initialize TweakControllerCompact", e);
        }
    }

}
