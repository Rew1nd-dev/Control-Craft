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

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public abstract class CimulinkSerialization implements ICimulinkSerialization {
    protected final CompactVersion version;

    public CimulinkSerialization(CompactVersion version) {
        this.version = version;
    }

    @Override
    public final CompoundTag onCopy(
            @NotNull ServerLevel serverLevel,
            @NotNull BlockPos blockPos,
            @NotNull BlockState blockState,
            @Nullable BlockEntity blockEntity,
            @NotNull List<? extends ServerShip> list,
            @NotNull Map<Long, ? extends Vector3d> map
    ) {
        CompoundTag tag = onCopyImpl(serverLevel, blockPos, blockState, blockEntity, list, map);
        if(tag != null){
            tag.put("version", SerializeUtils.ofEnum(CompactVersion.class).serialize(version));
        }
        return tag;
    }

    public abstract @Nullable CompoundTag onCopyImpl(
            @NotNull ServerLevel serverLevel,
            @NotNull BlockPos blockPos,
            @NotNull BlockState blockState,
            @Nullable BlockEntity blockEntity,
            @NotNull List<? extends ServerShip> list,
            @NotNull Map<Long, ? extends Vector3d> map
    );


    public static class V0 extends CimulinkSerialization {

        public V0() {
            super(CompactVersion.V0);
        }

        @Override
        public @Nullable CompoundTag onCopyImpl(@NotNull ServerLevel serverLevel, @NotNull BlockPos blockPos, @NotNull BlockState blockState, @Nullable BlockEntity blockEntity, @NotNull List<? extends ServerShip> list, @NotNull Map<Long, ? extends Vector3d> map) {
            if(blockEntity instanceof CimulinkBlockEntity<?> cimulink){
                return VSchematicCompactCimulinkV0.PreWriteCimulinkVModCompact(cimulink);
            }
            return null;
        }

        @Override
        public CompoundTag onPaste(
                @NotNull ServerLevel serverLevel,
                @NotNull BlockPos blockPos,
                @NotNull BlockState blockState,
                @NotNull Map<Long, Long> map,
                @NotNull Map<Long, ? extends Pair<? extends Vector3d, ? extends Vector3d>> map1,
                @Nullable CompoundTag compoundTag
        ) {
            return VSchematicCompactCimulinkV0.PreCimulinkReadVModCompact(
                    serverLevel,
                    map,
                    map1,
                    compoundTag
            );
        }

        @Override
        public void finalize(@NotNull CimulinkBlockEntity<?> cbe, @NotNull CompoundTag modifiedTag) {
            VSchematicCompactCimulinkV0.PostCimulinkReadVModCompact(cbe, modifiedTag);
        }

    }

    public static class V1 extends CimulinkSerialization {

        public V1() {
            super(CompactVersion.V1);
        }

        @Override
        public @Nullable CompoundTag onCopyImpl(@NotNull ServerLevel serverLevel, @NotNull BlockPos blockPos, @NotNull BlockState blockState, @Nullable BlockEntity blockEntity, @NotNull List<? extends ServerShip> list, @NotNull Map<Long, ? extends Vector3d> map) {
            if(blockEntity instanceof CimulinkBlockEntity<?> cimulink){
                return VSchematicCompactCimulinkV1.PreWriteCimulinkVModCompact(cimulink);
            }
            return null;
        }

        @Override
        public CompoundTag onPaste(
                @NotNull ServerLevel serverLevel,
                @NotNull BlockPos blockPos,
                @NotNull BlockState blockState,
                @NotNull Map<Long, Long> map,
                @NotNull Map<Long, ? extends Pair<? extends Vector3d, ? extends Vector3d>> map1,
                @Nullable CompoundTag compoundTag
        ) {
            return VSchematicCompactCimulinkV1.PreCimulinkReadVModCompact(
                    serverLevel,
                    map,
                    map1,
                    compoundTag
            );
        }

        @Override
        public void finalize(@NotNull CimulinkBlockEntity<?> cbe, @NotNull CompoundTag modifiedTag) {
            VSchematicCompactCimulinkV1.PostCimulinkReadVModCompact(cbe, modifiedTag);
        }

    }


}
