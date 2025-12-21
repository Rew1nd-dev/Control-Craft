package com.verr1.controlcraft.content.compact.vmod.version;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.valkyrienskies.core.api.ships.Ship;

import java.util.Map;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class VSchematicCompactCimulinkV0 {

    public static @Nullable CompoundTag PreWriteCimulinkVModCompact(CimulinkBlockEntity<?> cbe){
        CompoundTag linkOriginal = new CompoundTag();
        CompoundTag compact = new CompoundTag();
        cbe.writeCompact(linkOriginal);
        Ship self = cbe.getShipOn();
        if(self == null)return null;
        compact.putLong("o_self_ID", self.getId());

        linkOriginal.put("compact", compact);
        return linkOriginal;
    }

    public static CompoundTag PreCimulinkReadVModCompact(
            @NotNull ServerLevel serverLevel,
            @NotNull Map<Long, Long> map,
            @NotNull Map<Long, ? extends Pair<? extends Vector3d, ? extends Vector3d>> offsetMap,
            @Nullable CompoundTag tagToModify
    ){

        ControlCraft.LOGGER.debug("PreCimulinkReadVModCompact"); //, tagToModify
        ControlCraft.LOGGER.debug("lMap: {}", map);

        if(tagToModify == null)return null;
        CompoundTag compact = tagToModify.getCompound("compact");
        long o_self_id = compact.getLong("o_self_ID");
        if(!map.containsKey(o_self_id))return tagToModify;
        long n_self_id = map.get(o_self_id);

        ControlCraft.LOGGER.debug("link has new ship {}", n_self_id);

        Vector3d oldCenter = offsetMap.get(o_self_id).getFirst();
        Vector3d newCenter = offsetMap.get(o_self_id).getSecond();
        compact.putLong("offset", BlockPos.containing(
                        toMinecraft(
                                newCenter.sub(oldCenter, new Vector3d()))
                ).asLong()
        );

        ControlCraft.LOGGER.debug("PreCimulinkReadVModCompact Modified:"); //, tagToModify
        return tagToModify;
    }


    public static void PostCimulinkReadVModCompact(CimulinkBlockEntity<?> cbe, CompoundTag modifiedTag){
        CompoundTag compact = modifiedTag.getCompound("compact");

        ControlCraft.LOGGER.debug(".PostCimulinkReadVModCompact"); //, compact

        if(!compact.contains("offset"))return;

        BlockPos offset = BlockPos.of(compact.getLong("offset"));
        cbe.linkPort().modifyWithOffset(offset);
        cbe.setChanged();
    }

}
