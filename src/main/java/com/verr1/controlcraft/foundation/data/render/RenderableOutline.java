package com.verr1.controlcraft.foundation.data.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.simibubi.create.foundation.render.SuperRenderTypeBuffer;
import net.minecraft.world.phys.Vec3;

public interface RenderableOutline {
    void render(PoseStack ms, SuperRenderTypeBuffer buffer, Vec3 camera);
    void tick();
}
