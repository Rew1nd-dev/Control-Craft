package com.verr1.controlcraft.hooks;

import com.verr1.controlcraft.registry.ControlCraftEntities;
import net.minecraft.world.entity.Entity;
import org.spongepowered.asm.mixin.Unique;

public class NoCullEntityHooks {
    @Unique
    public static boolean shouldNoCull(Entity entity) {
        return entity.getType().equals(ControlCraftEntities.FLAP.get());
    }
}
