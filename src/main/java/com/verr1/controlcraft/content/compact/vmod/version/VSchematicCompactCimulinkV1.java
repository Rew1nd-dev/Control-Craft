package com.verr1.controlcraft.content.compact.vmod.version;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.links.BlockPort;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.mod.common.VSGameUtilsKt;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class VSchematicCompactCimulinkV1 {

    public static BlockPos centerPosOf(int x, int z){
        return new BlockPos(((x / 16 / 256 - 1) * 256 + 128) * 16, 0, ((z / 16 / 256) * 256 + 128) * 16);
    }

    private static Map<Long, Pair<Vector3d, Vector3d>> cast(Map<Long, ? extends Pair<? extends Vector3d, ? extends Vector3d>> m){
        Map<Long, Pair<Vector3d, Vector3d>> result = new HashMap<>();
        m.forEach((k, v) -> result.put(k, new Pair<>(v.getFirst(), v.getSecond())));
        return result;
    }

    private static Map<BlockPos, Long> convert(List<CenterAndId> cni){
        Map<BlockPos, Long> result = new HashMap<>();
        cni.forEach(e -> result.put(e.center, e.id));
        return result;
    }

    public static @Nullable CompoundTag PreWriteCimulinkVModCompact(CimulinkBlockEntity<?> cbe){
        CompoundTag linkOriginal = new CompoundTag();
        CompoundTag compact = new CompoundTag();
        cbe.writeCompact(linkOriginal);
        Ship self = cbe.getShipOn();
        if(self == null)return null;

        List<CenterAndId> cni = cbe.collectVModCompact();

        compact.put("cni", CenterAndId.SERS.serialize(cni));

        linkOriginal.put("compact", compact);
        return linkOriginal;
    }

    public static CompoundTag PreCimulinkReadVModCompact(
            @NotNull ServerLevel serverLevel,
            @NotNull Map<Long, Long> map,
            @NotNull Map<Long, ? extends Pair<? extends Vector3d, ? extends Vector3d>> offsetMap,
            @Nullable CompoundTag tagToModify
    ){
        if(tagToModify == null || !tagToModify.contains("compact"))return null;

        OffsetMaps oMaps = new OffsetMaps(map, cast(offsetMap));
        tagToModify.getCompound("compact").put("mapping", oMaps.serialize());

        return tagToModify;
    }



    public static void PostCimulinkReadVModCompact(CimulinkBlockEntity<?> cbe, CompoundTag modifiedTag){

        if(!modifiedTag.contains("compact"))return;
        CompoundTag compact = modifiedTag.getCompound("compact");
        OffsetMaps oMaps = OffsetMaps.deserialize(compact.getCompound("mapping"));
        Map<BlockPos, Long> cMaps = convert(CenterAndId.SERS.deserialize(compact.getCompound("cni")));

        Function<BlockPos, BlockPos> offsetCompute = oldBlockPos -> {
            BlockPos oldCenter = centerPosOf(oldBlockPos.getX(), oldBlockPos.getZ());
            Long oldId = cMaps.get(oldCenter);
            if(oldId == null)return BlockPos.ZERO;
            Long newId = oMaps.idMap.get(oldId);
            if(newId == null)return BlockPos.ZERO;
            Pair<Vector3d, Vector3d> offset = oMaps.centerMaps.get(oldId);
            if(offset == null)return BlockPos.ZERO;
            Vector3dc old_center = offset.getFirst();
            Vector3dc new_center = offset.getSecond();
            if(old_center == null || new_center == null)return BlockPos.ZERO;


            return BlockPos.containing(toMinecraft(new_center.sub(old_center, new Vector3d())));
        };


        cbe.linkPort().modifyWithOffset(offsetCompute);
        cbe.setChanged();
    }

    public record CenterAndId(long id, BlockPos center){
        public static Serializer<List<CenterAndId>> SERS = SerializeUtils.ofList(SerializeUtils.of(
                CenterAndId::serialize,
                CenterAndId::deserialize
        ));

        public static CenterAndId of(BlockPort blockPort){
            WorldBlockPos pos = blockPort.pos();
            BlockPos blockPos = pos.pos();
            ServerLevel level = pos.level(ControlCraftServer.INSTANCE);
            Ship s = VSGameUtilsKt.getShipManagingPos(level, blockPos);
            long id = s == null ? -1L : s.getId();
            BlockPos center = centerPosOf(blockPos.getX(), blockPos.getZ());
            return new CenterAndId(id, center);
        }

        public CompoundTag serialize(){
            return CompoundTagBuilder.create()
                    .withCompound("id", SerializeUtils.LONG.serialize(id))
                    .withCompound("center", SerializeUtils.BLOCK_POS.serialize(center))
                    .build();
        }

        public static CenterAndId deserialize(CompoundTag tag){
            return new CenterAndId(
                    SerializeUtils.LONG.deserialize(tag.getCompound("id")),
                    SerializeUtils.BLOCK_POS.deserialize(tag.getCompound("center"))
            );
        }


    }



    public record OffsetMaps(
            Map<Long, Long> idMap,
            Map<Long, Pair<Vector3d, Vector3d>> centerMaps
    ){
        static final Serializer<Map<Long, Long>> SER_ID = SerializeUtils.ofMap(
                SerializeUtils.LONG,
                SerializeUtils.LONG
        );
        static final Serializer<Map<Long, Pair<Vector3d, Vector3d>>> SER_CENTER = SerializeUtils.ofMap(
                SerializeUtils.LONG,
                SerializeUtils.ofPair(SerializeUtils.VECTOR3D)
        );

        public CompoundTag serialize(){
            return CompoundTagBuilder.create()
                    .withCompound("id", SER_ID.serialize(idMap))
                    .withCompound("center", SER_CENTER.serialize(centerMaps))
                    .build();
        }

        public static OffsetMaps deserialize(CompoundTag tag){
            return new OffsetMaps(
                    SER_ID.deserialize(tag.getCompound("id")),
                    SER_CENTER.deserialize(tag.getCompound("center"))
            );
        }

    }

}
