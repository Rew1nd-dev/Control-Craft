package com.verr1.controlcraft.content.compact.vmod;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.motor.AbstractMotor;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.Ship;

import java.util.Map;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class VSchematicCompactCenter {

    public static @Nullable CompoundTag PreWriteMotorVModCompact(AbstractMotor motor){
        CompoundTag motorOriginal = new CompoundTag();
        CompoundTag compact = new CompoundTag();
        motor.writeCompact(motorOriginal);
        Ship comp = motor.getCompanionServerShip();
        if(comp == null || motor.blockConnectContext().equals(BlockPos.ZERO))return null;
        compact.putLong("o_comp_ID", comp.getId());
        compact.put("direction", SerializeUtils.ofEnum(Direction.class).serialize(motor.getCompanionShipAlign()));
        motorOriginal.put("compact", compact);
        return motorOriginal;
    }

    public static CompoundTag PreMotorReadVModCompact(
            @NotNull ServerLevel serverLevel,
            @NotNull Map<Long, Long> map,
            @NotNull Map<Long, ? extends Pair<? extends Vector3d, ? extends Vector3d>> offsetMap,
            @Nullable CompoundTag tagToModify
    ){

        ControlCraft.LOGGER.debug("PreMotorReadVModCompact: "); // + tagToModify
        ControlCraft.LOGGER.debug("Map: " + map);



        if(tagToModify == null)return null;
        CompoundTag compact = tagToModify.getCompound("compact");
        long o_comp_id = compact.getLong("o_comp_ID");
        if(!map.containsKey(o_comp_id))return tagToModify;
        long n_comp_id = map.get(o_comp_id);

        ControlCraft.LOGGER.debug("has n ship {}", n_comp_id);

        // Ship n_comp = ValkyrienSkies.getShipWorld(serverLevel).getAllShips().getById(n_comp_id);

        Vector3d oldCenter = offsetMap.get(o_comp_id).getFirst();
        Vector3d newCenter = offsetMap.get(o_comp_id).getSecond();



        tagToModify.getCompound("compact").putLong("offset", BlockPos.containing(
                toMinecraft(
                        newCenter.sub(oldCenter, new Vector3d()))
                ).asLong()
        );

        ControlCraft.LOGGER.debug("PreMotorReadVModCompact Modified: "); // + tagToModify
        return tagToModify;
    }


    public static void PostMotorReadVModCompact(AbstractMotor motor, CompoundTag tag){
        CompoundTag compact = tag.getCompound("compact");

        ControlCraft.LOGGER.debug("PostMotorReadVModCompact: ");

        if(!compact.contains("offset"))return;

        Direction direction = !compact.contains("direction") ?
                motor.getCompanionShipAlign()
                        :
                SerializeUtils.ofEnum(Direction.class).deserialize(compact.getCompound("direction"));

        BlockPos connectContext = motor.blockConnectContext();
        motor.setCompanionShipID(-1);

        BlockPos offset = BlockPos.of(compact.getLong("offset"));
        BlockPos newContact = connectContext.offset(offset);

        ControlCraft.LOGGER.debug("PostMotorReadVModCompact: {} {}", motor.blockConnectContext(), newContact);

        ControlCraftServer.SERVER_EXECUTOR.executeLater(() -> motor.bruteDirectionalConnectWith(newContact, Direction.UP, direction), 12);

    }







    public static Vector3d centerVecOf(int x, int z){
        int cx = ((x / 16 / 256 - 1) * 256 + 128) * 16;
        int cz = ((z / 16 / 256) * 256 + 128) * 16;
        return new Vector3d(cx, 0, cz);
    }




}
