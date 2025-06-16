package com.verr1.controlcraft.content.compact.vmod;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.motor.AbstractMotor;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import kotlin.Pair;
import kotlin.Unit;
import kotlin.jvm.functions.Function1;
import kotlin.jvm.functions.Function2;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.spaceeye.valkyrien_ship_schematics.interfaces.ICopyableBlock;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.List;
import java.util.Map;

public interface CopyableCimulink extends ICopyableBlock {


    @Nullable
    @Override
    default CompoundTag onCopy(
            @NotNull ServerLevel serverLevel,
            @NotNull BlockPos blockPos,
            @NotNull BlockState blockState,
            @Nullable BlockEntity blockEntity,
            @NotNull List<? extends ServerShip> list,
            @NotNull Map<Long, ? extends Vector3d> map
    ){
        ControlCraft.LOGGER.info("On Copy Called");
        if(!(blockEntity instanceof CimulinkBlockEntity<?> cimulink)) return null;
        return VSchematicCompactCenter.PreWriteCimulinkVModCompact(cimulink);
    }


    @Nullable
    @Override
    default CompoundTag onPaste(
            @NotNull ServerLevel serverLevel,
            @NotNull BlockPos blockPos,
            @NotNull BlockState blockState,
            @NotNull Map<Long, Long> map,
            @NotNull Map<Long, ? extends Pair<? extends Vector3d, ? extends Vector3d>> map1,
            @Nullable CompoundTag compoundTag
    ){
        return VSchematicCompactCenter.PreCimulinkReadVModCompact(serverLevel, map, map1, compoundTag);
    };





}
