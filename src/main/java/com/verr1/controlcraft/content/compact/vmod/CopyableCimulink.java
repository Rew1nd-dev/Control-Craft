package com.verr1.controlcraft.content.compact.vmod;

import com.verr1.controlcraft.content.compact.vmod.version.CimulinkSerializations;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.mod.common.assembly.ICopyableBlock;

import java.util.List;
import java.util.Map;

public interface CopyableCimulink extends ICopyableBlock {

    @Override
    @Nullable default CompoundTag onCopy(@NotNull ServerLevel serverLevel, @NotNull BlockPos blockPos, @NotNull BlockState blockState, @Nullable BlockEntity blockEntity, @NotNull List<? extends ServerShip> list, @NotNull Map<Long, ? extends Vector3dc> map) {
        return CimulinkSerializations.INSTANCE.onCopy(serverLevel, blockPos, blockState, blockEntity, list, map);
    }



    @Nullable
    @Override
    default CompoundTag onPaste(
            @NotNull ServerLevel serverLevel,
            @NotNull BlockPos blockPos,
            @NotNull BlockState blockState,
            @NotNull Map<Long, Long> map,
            @NotNull Map<Long, ? extends Pair<? extends Vector3dc, ? extends Vector3dc>> map1,
            @Nullable CompoundTag compoundTag
    ){
        return CimulinkSerializations.INSTANCE.onPaste(serverLevel, blockPos, blockState, map, map1, compoundTag);
    }





}
