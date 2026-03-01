package com.verr1.controlcraft.registry;

import com.simibubi.create.Create;
import com.simibubi.create.content.contraptions.AbstractContraptionEntity;
import com.simibubi.create.content.contraptions.OrientedContraptionEntity;
import com.simibubi.create.content.contraptions.render.OrientedContraptionEntityRenderer;
import com.simibubi.create.foundation.data.CreateEntityBuilder;
import com.simibubi.create.foundation.utility.Lang;
import com.tterrag.registrate.util.entry.EntityEntry;
import com.tterrag.registrate.util.nullness.NonNullConsumer;
import com.tterrag.registrate.util.nullness.NonNullFunction;
import com.tterrag.registrate.util.nullness.NonNullSupplier;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.flap.FlapContraptionEntity;
import com.verr1.controlcraft.render.FlapContraptionEntityRenderer;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobCategory;

public class ControlCraftEntities {

    public static final EntityEntry<FlapContraptionEntity> FLAP = contraption("flap_contraption",
        FlapContraptionEntity::new, () -> FlapContraptionEntityRenderer::new, 5, 3, true).register();

    private static <T extends Entity> CreateEntityBuilder<T, ?> contraption(String name, EntityType.EntityFactory<T> factory,
                                                                            NonNullSupplier<NonNullFunction<EntityRendererProvider.Context, EntityRenderer<? super T>>> renderer, int range,
                                                                            int updateFrequency, boolean sendVelocity) {
        return register(name, factory, renderer, MobCategory.MISC, range, updateFrequency, sendVelocity, true,
            AbstractContraptionEntity::build);
    }

    private static <T extends Entity> CreateEntityBuilder<T, ?> register(String name, EntityType.EntityFactory<T> factory,
                                                                         NonNullSupplier<NonNullFunction<EntityRendererProvider.Context, EntityRenderer<? super T>>> renderer,
                                                                         MobCategory group, int range, int updateFrequency, boolean sendVelocity, boolean immuneToFire,
                                                                         NonNullConsumer<EntityType.Builder<T>> propertyBuilder) {
        String id = Lang.asId(name);
        return (CreateEntityBuilder<T, ?>) ControlCraft.REGISTRATE
            .entity(id, factory, group)
            .properties(b -> b.setTrackingRange(range)
                .setUpdateInterval(updateFrequency)
                .setShouldReceiveVelocityUpdates(sendVelocity))
            .properties(propertyBuilder)
            .properties(b -> {
                if (immuneToFire)
                    b.fireImmune();
            })
            .renderer(renderer);
    }

    public static void register(){

    }
}
