package com.verr1.controlcraft.content.compact.vmod.version;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.utils.SerializeUtils;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.List;
import java.util.Map;

public class CimulinkSerializations implements ICimulinkSerialization{
    private static final CimulinkSerialization.V0 v0 = new CimulinkSerialization.V0();
    private static final CimulinkSerialization.V1 v1 = new CimulinkSerialization.V1();

    public static final CimulinkSerializations INSTANCE = new CimulinkSerializations();

    @Override
    public CompoundTag onCopy(@NotNull ServerLevel serverLevel, @NotNull BlockPos blockPos, @NotNull BlockState blockState, @Nullable BlockEntity blockEntity, @NotNull List<? extends ServerShip> list, @NotNull Map<Long, ? extends Vector3d> map) {
        return v1.onCopy(serverLevel, blockPos, blockState, blockEntity, list, map);
    }

    @Override
    public CompoundTag onPaste(@NotNull ServerLevel serverLevel, @NotNull BlockPos blockPos, @NotNull BlockState blockState, @NotNull Map<Long, Long> map, @NotNull Map<Long, ? extends Pair<? extends Vector3d, ? extends Vector3d>> map1, @Nullable CompoundTag compoundTag) {
        if(compoundTag == null)return null;
        CompactVersion version = fetch(compoundTag);

        return (switch (version){
            case V0 -> v0;
            case V1 -> v1;
        })
        .onPaste(serverLevel, blockPos, blockState, map, map1, compoundTag);
    }

    @Override
    public void finalize(@NotNull CimulinkBlockEntity<?> cbe, @NotNull CompoundTag modifiedTag) {
        CompactVersion version = fetch(modifiedTag);
        (switch (version){
            case V0 -> v0;
            case V1 -> v1;
        })
        .finalize(cbe, modifiedTag);
    }


    private static CompactVersion fetch(CompoundTag tag){
        return tag.contains("version") ?
                SerializeUtils.ofEnum(CompactVersion.class).deserialize(tag.getCompound("version"))
                :
                CompactVersion.V0;
    }
}
