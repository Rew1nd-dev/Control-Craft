package com.verr1.controlcraft.foundation.data.links;

import com.simibubi.create.foundation.blockEntity.behaviour.ValueBoxTransform;
import com.simibubi.create.foundation.utility.VecHelper;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.Vec3;

public class LinkPortSlot extends ValueBoxTransform.Sided {
    // transform is set in ValueBox, and state is Blockstate at ValueBox::pos

    private float y = 8;
    private float x = 3;

    public LinkPortSlot(float x, float y) {
        this.y = y;
        this.x = x;
    }


    @Override
    protected Vec3 getSouthLocation() {
        int signY = direction == Direction.DOWN ? -1 : 1;
        int signX = direction == Direction.DOWN ? 1 : direction.getAxisDirection() == Direction.AxisDirection.POSITIVE ? 1 : -1;
        return VecHelper.voxelSpace(8 + signX * x, 8 + signY * y, 8);
    }

}
