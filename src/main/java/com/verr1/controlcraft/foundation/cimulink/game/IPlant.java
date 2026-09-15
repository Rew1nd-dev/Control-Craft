package com.verr1.controlcraft.foundation.cimulink.game;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import net.minecraft.core.Direction;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraftforge.registries.ForgeRegistries;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public interface IPlant extends PlantMetadataProvider {

    @NotNull NamedComponent plant();

    @Override
    default ResourceLocation plantType() {
        if (this instanceof BlockEntity blockEntity) {
            return Optional
                    .ofNullable(ForgeRegistries.BLOCK_ENTITY_TYPES.getKey(blockEntity.getType()))
                    .orElseGet(() -> ControlCraft.asResource("unknown"));
        }
        return PlantMetadataProvider.super.plantType();
    }

    @Override
    default Map<String, Object> plantMetadata() {
        if (!(this instanceof BlockEntity blockEntity)) {
            return PlantMetadataProvider.super.plantMetadata();
        }

        Map<String, Object> metadata = new HashMap<>();
        Optional.ofNullable(ForgeRegistries.BLOCKS.getKey(blockEntity.getBlockState().getBlock()))
                .ifPresent(key -> metadata.put("block", key.toString()));
        Optional.ofNullable(ForgeRegistries.BLOCK_ENTITY_TYPES.getKey(blockEntity.getType()))
                .ifPresent(key -> metadata.put("blockEntity", key.toString()));

        BlockState state = blockEntity.getBlockState();
        Direction direction = null;
        if (state.hasProperty(BlockStateProperties.FACING)) {
            direction = state.getValue(BlockStateProperties.FACING);
        } else if (state.hasProperty(BlockStateProperties.HORIZONTAL_FACING)) {
            direction = state.getValue(BlockStateProperties.HORIZONTAL_FACING);
        }

        if (direction != null) {
            metadata.put("direction", direction.getName());
            metadata.put("axis", direction.getAxis().getName());
        } else if (state.hasProperty(BlockStateProperties.AXIS)) {
            metadata.put("axis", state.getValue(BlockStateProperties.AXIS).getName());
        }

        return Map.copyOf(metadata);
    }

    default String getName(){
        return plant().name();
    }

    default void setName(String name){
        plant().withName(name);
    }

}
