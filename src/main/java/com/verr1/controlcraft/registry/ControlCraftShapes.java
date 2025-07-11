package com.verr1.controlcraft.registry;

import com.simibubi.create.AllShapes;
import com.simibubi.create.foundation.utility.VoxelShaper;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.phys.shapes.VoxelShape;

public class ControlCraftShapes {
    public static final VoxelShape
            HALF_BOX_SHAPE = cuboid(0, 0, 0, 16, 8, 16),
            CENTRAL_UNIT_SHAPE = cuboid(7, 7, 7, 9, 9, 9),
            SMALL_ROD_SHAPE = cuboid(2, 0, 2, 14, 10, 14),
            SCOPE_SHAPE = cuboid(1, 1, 1, 15, 13, 16),
            FLAT_BASE_SHAPE = cuboid(0, 0, 0, 16, 4, 16)
                    ;


    public static final VoxelShaper
            HALF_BOX_BASE = shape(HALF_BOX_SHAPE).forDirectional(),
            DIRECTIONAL_ROD = shape(SMALL_ROD_SHAPE).forDirectional(),
            SCOPE_SHAPE_FACE = shape(SCOPE_SHAPE).forDirectional(),
            FLAT_BASE = shape(FLAT_BASE_SHAPE).forDirectional()
                    ;

    private static VoxelShape cuboid(double x1, double y1, double z1, double x2, double y2, double z2) {
        return Block.box(x1, y1, z1, x2, y2, z2);
    }

    private static AllShapes.Builder shape(VoxelShape shape) {
        return new AllShapes.Builder(shape);
    }
}
