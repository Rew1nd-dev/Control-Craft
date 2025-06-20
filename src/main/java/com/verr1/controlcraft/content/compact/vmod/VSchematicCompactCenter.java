package com.verr1.controlcraft.content.compact.vmod;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.motor.AbstractMotor;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
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
        motorOriginal.put("compact", compact);
        return motorOriginal;
    }

    /*
    public static @Nullable CompoundTag PreWriteMotorVModCompact(AbstractMotor motor){
        CompoundTag motorOriginal = new CompoundTag();
        CompoundTag compact = new CompoundTag();
        motor.writeCompact(motorOriginal);
        Ship comp = motor.getCompanionServerShip();
        if(comp == null || motor.blockConnectContext().equals(BlockPos.ZERO))return null;
        int xM = motor.blockConnectContext().getX();
        int zM = motor.blockConnectContext().getZ();
        ControlCraft.LOGGER.info("PreWriteMotorVModCompact: " + xM + " " + zM);
        BlockPos center_xz_old = centerPosOf(xM, zM);
        compact.putLong("o_comp_chunk_center", center_xz_old.asLong());
        compact.putLong("o_comp_ID", comp.getId());

        motorOriginal.put("compact", compact);
        return motorOriginal;
    }
    public static void PostMotorReadVModCompact(AbstractMotor motor, CompoundTag tag){
        CompoundTag compact = tag.getCompound("compact");

        ControlCraft.LOGGER.info("PostMotorReadVModCompact: " + compact);

        if(!compact.contains("n_comp_chunk_center") || !compact.contains("o_comp_chunk_center"))return;
        BlockPos comp_center_old = BlockPos.of(compact.getLong("o_comp_chunk_center"));
        BlockPos comp_center_new = BlockPos.of(compact.getLong("n_comp_chunk_center"));

        BlockPos connectContext = motor.blockConnectContext();
        motor.setCompanionShipID(-1);

        ControlCraft.LOGGER.info("PostMotorReadVModCompact: {} {}", comp_center_old, comp_center_new);

        BlockPos offset = comp_center_new.subtract(comp_center_old);
        BlockPos newContact = connectContext.offset(offset);

        ControlCraft.LOGGER.info("PostMotorReadVModCompact: {} {}", motor.blockConnectContext(), newContact);

        ControlCraftServer.SERVER_EXECUTOR.executeLater(() -> motor.bruteDirectionalConnectWith(newContact, Direction.UP, motor.getCompanionShipAlign()), 12);

    }
    * public static CompoundTag __PreMotorReadVModCompact(
            @NotNull ServerLevel serverLevel,
            @NotNull Map<Long, Long> map,
            @NotNull Map<Long, ? extends Pair<? extends Vector3d, ? extends Vector3d>> offsetMap,
            @Nullable CompoundTag tagToModify
    ){

        ControlCraft.LOGGER.info("PreMotorReadVModCompact: " + tagToModify);
        ControlCraft.LOGGER.info("Map: " + map);

        if(tagToModify == null)return null;
        CompoundTag compact = tagToModify.getCompound("compact");
        long o_comp_id = compact.getLong("o_comp_ID");
        if(!map.containsKey(o_comp_id))return tagToModify;
        long n_comp_id = map.get(o_comp_id);

        ControlCraft.LOGGER.info("has n ship {}", n_comp_id);

        Ship n_comp = ValkyrienSkies.getShipWorld(serverLevel).getAllShips().getById(n_comp_id);
        if(n_comp == null)return tagToModify;

        ControlCraft.LOGGER.info("found n ship");

        int xM = n_comp.getChunkClaim().getXMiddle() * 16;
        int zM = n_comp.getChunkClaim().getZMiddle() * 16;

        BlockPos center_xz_new = centerPosOf(xM, zM);

        tagToModify.getCompound("compact").putLong("n_comp_chunk_center", center_xz_new.asLong());

        ControlCraft.LOGGER.info("PreMotorReadVModCompact Modified: " + tagToModify);
        return tagToModify;
    }
    * */

    public static CompoundTag PreMotorReadVModCompact(
            @NotNull ServerLevel serverLevel,
            @NotNull Map<Long, Long> map,
            @NotNull Map<Long, ? extends Pair<? extends Vector3d, ? extends Vector3d>> offsetMap,
            @Nullable CompoundTag tagToModify
    ){

        ControlCraft.LOGGER.info("PreMotorReadVModCompact: " + tagToModify);
        ControlCraft.LOGGER.info("Map: " + map);



        if(tagToModify == null)return null;
        CompoundTag compact = tagToModify.getCompound("compact");
        long o_comp_id = compact.getLong("o_comp_ID");
        if(!map.containsKey(o_comp_id))return tagToModify;
        long n_comp_id = map.get(o_comp_id);

        ControlCraft.LOGGER.info("has n ship {}", n_comp_id);

        // Ship n_comp = ValkyrienSkies.getShipWorld(serverLevel).getAllShips().getById(n_comp_id);

        Vector3d oldCenter = offsetMap.get(o_comp_id).getFirst();
        Vector3d newCenter = offsetMap.get(o_comp_id).getSecond();



        tagToModify.getCompound("compact").putLong("offset", BlockPos.containing(
                toMinecraft(
                        newCenter.sub(oldCenter, new Vector3d()))
                ).asLong()
        );

        ControlCraft.LOGGER.info("PreMotorReadVModCompact Modified: " + tagToModify);
        return tagToModify;
    }


    public static void PostMotorReadVModCompact(AbstractMotor motor, CompoundTag tag){
        CompoundTag compact = tag.getCompound("compact");

        ControlCraft.LOGGER.info("PostMotorReadVModCompact: " + compact);

        if(!compact.contains("offset"))return;

        BlockPos connectContext = motor.blockConnectContext();
        motor.setCompanionShipID(-1);

        BlockPos offset = BlockPos.of(compact.getLong("offset"));
        BlockPos newContact = connectContext.offset(offset);

        ControlCraft.LOGGER.info("PostMotorReadVModCompact: {} {}", motor.blockConnectContext(), newContact);

        ControlCraftServer.SERVER_EXECUTOR.executeLater(() -> motor.bruteDirectionalConnectWith(newContact, Direction.UP, motor.getCompanionShipAlign()), 12);

    }

    /*
    *
    * public static @Nullable CompoundTag PreWriteCimulinkVModCompact(CimulinkBlockEntity<?> motor){
        CompoundTag linkOriginal = new CompoundTag();
        CompoundTag compact = new CompoundTag();
        motor.writeCompact(linkOriginal);
        Ship self = motor.getShipOn();
        if(self == null)return null;
        int xM = motor.getBlockPos().getX();
        int zM = motor.getBlockPos().getZ();
        ControlCraft.LOGGER.info("PreWriteCimulinkVModCompact: {} {}", xM, zM);
        BlockPos center_xz_old = centerPosOf(xM, zM);
        compact.putLong("o_self_chunk_center", center_xz_old.asLong());
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

        ControlCraft.LOGGER.info("PreCimulinkReadVModCompact: {}", tagToModify);
        ControlCraft.LOGGER.info("lMap: {}", map);

        if(tagToModify == null)return null;
        CompoundTag compact = tagToModify.getCompound("compact");
        long o_self_id = compact.getLong("o_self_ID");
        if(!map.containsKey(o_self_id))return tagToModify;
        long n_self_id = map.get(o_self_id);

        ControlCraft.LOGGER.info("link has new ship {}", n_self_id);

        Ship n_self = ValkyrienSkies.getShipWorld(serverLevel).getAllShips().getById(n_self_id);
        if(n_self == null)return tagToModify;

        ControlCraft.LOGGER.info("link found new ship");

        int xM = n_self.getChunkClaim().getXMiddle() * 16;
        int zM = n_self.getChunkClaim().getZMiddle() * 16;

        BlockPos center_xz_new = centerPosOf(xM, zM);

        compact.putLong("n_self_chunk_center", center_xz_new.asLong());

        ControlCraft.LOGGER.info("PreCimulinkReadVModCompact Modified: {}", tagToModify);
        return tagToModify;
    }

    public static void PostCimulinkReadVModCompact(CimulinkBlockEntity<?> cbe, CompoundTag modifiedTag){
        CompoundTag compact = modifiedTag.getCompound("compact");

        ControlCraft.LOGGER.info("PostCimulinkReadVModCompact: {}", compact);

        if(!compact.contains("n_self_chunk_center") || !compact.contains("o_self_chunk_center"))return;
        BlockPos self_center_old = BlockPos.of(compact.getLong("o_self_chunk_center"));
        BlockPos self_center_new = BlockPos.of(compact.getLong("n_self_chunk_center"));

        BlockPos offset = self_center_new.subtract(self_center_old);
        cbe.linkPort().modifyWithOffset(offset);

    }
    * */

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

        ControlCraft.LOGGER.info("PreCimulinkReadVModCompact: {}", tagToModify);
        ControlCraft.LOGGER.info("lMap: {}", map);

        if(tagToModify == null)return null;
        CompoundTag compact = tagToModify.getCompound("compact");
        long o_self_id = compact.getLong("o_self_ID");
        if(!map.containsKey(o_self_id))return tagToModify;
        long n_self_id = map.get(o_self_id);

        ControlCraft.LOGGER.info("link has new ship {}", n_self_id);

        Vector3d oldCenter = offsetMap.get(o_self_id).getFirst();
        Vector3d newCenter = offsetMap.get(o_self_id).getSecond();
        compact.putLong("offset", BlockPos.containing(
                        toMinecraft(
                                newCenter.sub(oldCenter, new Vector3d()))
                ).asLong()
        );

        ControlCraft.LOGGER.info("PreCimulinkReadVModCompact Modified: {}", tagToModify);
        return tagToModify;
    }

    public static void PostCimulinkReadVModCompact(CimulinkBlockEntity<?> cbe, CompoundTag modifiedTag){
        CompoundTag compact = modifiedTag.getCompound("compact");

        ControlCraft.LOGGER.info("PostCimulinkReadVModCompact: {}", compact);

        if(!compact.contains("offset"))return;

        BlockPos offset = BlockPos.of(compact.getLong("offset"));
        cbe.linkPort().modifyWithOffset(offset);

    }

    public static BlockPos centerPosOf(int x, int z){
        return new BlockPos(((x / 16 / 256 - 1) * 256 + 128) * 16, 0, ((z / 16 / 256) * 256 + 128) * 16);
    }

}
