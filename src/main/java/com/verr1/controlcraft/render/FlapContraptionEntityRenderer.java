package com.verr1.controlcraft.render;

import com.simibubi.create.content.contraptions.ContraptionType;
import com.simibubi.create.content.contraptions.OrientedContraptionEntity;
import com.simibubi.create.content.contraptions.render.ContraptionEntityRenderer;
import com.verr1.controlcraft.content.blocks.flap.FlapContraptionEntity;
import net.minecraft.client.renderer.culling.Frustum;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import org.jetbrains.annotations.NotNull;

public class FlapContraptionEntityRenderer extends ContraptionEntityRenderer<FlapContraptionEntity> {
    public FlapContraptionEntityRenderer(EntityRendererProvider.Context context) {
        super(context);
    }

    @Override
    public boolean shouldRender(@NotNull FlapContraptionEntity entity, @NotNull Frustum p_225626_2_, double p_225626_3_,
                                double p_225626_5_, double p_225626_7_) {
        if (!super.shouldRender(entity, p_225626_2_, p_225626_3_, p_225626_5_, p_225626_7_))
            return false;
        return entity.getContraption()
            .getType() != ContraptionType.MOUNTED || entity.getVehicle() != null;
    }
}
