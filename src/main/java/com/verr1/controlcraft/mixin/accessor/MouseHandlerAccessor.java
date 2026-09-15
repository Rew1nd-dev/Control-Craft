package com.verr1.controlcraft.mixin.accessor;

import net.minecraft.client.MouseHandler;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(MouseHandler.class)
public interface MouseHandlerAccessor {
    @Accessor("isLeftPressed")
    void controlCraft$setLeftPressed(boolean value);

    @Accessor("isMiddlePressed")
    void controlCraft$setMiddlePressed(boolean value);

    @Accessor("isRightPressed")
    void controlCraft$setRightPressed(boolean value);
}
