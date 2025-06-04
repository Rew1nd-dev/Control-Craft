package com.verr1.controlcraft.content.links.shifter;

import com.verr1.controlcraft.content.links.CimulinkBlock;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;

public class ShifterLinkBlock extends CimulinkBlock<ShifterLinkBlockEntity> {

    protected ShifterLinkBlock(Properties p) {
        super(p);
    }

    @Override
    public void displayScreen(BlockPos p) {

    }

    @Override
    public Class<ShifterLinkBlockEntity> getBlockEntityClass() {
        return ShifterLinkBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends ShifterLinkBlockEntity> getBlockEntityType() {
        return null;
    }
}
