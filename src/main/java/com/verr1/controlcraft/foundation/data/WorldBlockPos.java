package com.verr1.controlcraft.foundation.data;

import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.core.GlobalPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.mod.common.VSGameUtilsKt;

import javax.annotation.Nullable;
import java.util.Objects;

public record WorldBlockPos(String dimensionID, BlockPos pos) {

    public static final WorldBlockPos NULL = new WorldBlockPos("null", BlockPos.ZERO);

    public static WorldBlockPos of(Level level, BlockPos pos){
        String d = VSGameUtilsKt.getDimensionId(level);
        return new WorldBlockPos(d, pos);
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        WorldBlockPos that = (WorldBlockPos) o;
        return Objects.equals(pos, that.pos) && Objects.equals(dimensionID, that.dimensionID);
    }

    @Override
    public int hashCode() {
        return Objects.hash(dimensionID, pos);
    }

    public GlobalPos globalPos(){
        return GlobalPos.of(key(), pos);
    }

    private ResourceKey<Level> key(){
        return VSGameUtilsKt.getResourceKey(dimensionID);
    }

    public @Nullable ServerLevel level(@NotNull MinecraftServer server){
        return server.getLevel(key());
    }


    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("pos", SerializeUtils.LONG.serialize(pos.asLong()))
                .withCompound("dimension", SerializeUtils.STRING.serialize(dimensionID))
                .build();
    }


    public static WorldBlockPos deserialize(CompoundTag tag){
        String dim = SerializeUtils.STRING.deserialize(tag.getCompound("dimension"));
        long posLong = SerializeUtils.LONG.deserialize(tag.getCompound("pos"));
        return new WorldBlockPos(dim, BlockPos.of(posLong));
    }

}
