package com.verr1.controlcraft.unstable.blocks.autocannon;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.compact.createbigcannons.APAutocannonAccess;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.unstable.ai.api.IAirCannon;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Objects;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class AiAutocannonBlockEntity extends OnShipBlockEntity implements IAirCannon {

    public AiAutocannonBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }


    @Override
    public int getCooldown() {
        return 0;
    }

    @Override
    public void fireAt(Vector3dc direction) {
        if(isClientSide())return;

        Objects.requireNonNull(level);
        Vector3dc p = readSelf().position();
        Vector3dc front = readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1));
        Vector3dc spawn = p.fma(10.0, front, new Vector3d());
        APAutocannonAccess ap = CreateBigCannonsCompact.createAutocannonAp(level);
        if(ap == null)return;
        ap.setPos(toMinecraft(spawn));
        ap.setTracer(true);
        ap.setLifetime(40);
        ap.shoot(direction.x(), direction.y(), direction.z(), 9, 0);
        ap.addToLevel();
    }
}
