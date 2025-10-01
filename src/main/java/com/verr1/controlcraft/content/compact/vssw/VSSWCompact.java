package com.verr1.controlcraft.content.compact.vssw;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraftforge.fml.ModList;
import org.jetbrains.annotations.Nullable;

public class VSSWCompact {

    static IVSSWCompactProxy getter;

    public static @Nullable NamedComponent vsswSeatPlant(ServerLevel level, BlockPos pos){
        if (getter == null)return null;
        return getter.getVSSWSeatPlant(level, pos);
    }


    public static void init(){
        if(!ModList.get().isLoaded("valkyrien_space_war"))return;
        // get constructor using reflect
        try {
            Class<?> clazz = Class.forName("com.verr1.controlcraft.content.compact.vssw.impl.VSSWCompactProxy");
            getter = (IVSSWCompactProxy) clazz.getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            ControlCraft.LOGGER.error("Failed to initialize VSSWCompact", e);
        }
    }

}
