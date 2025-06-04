package com.verr1.controlcraft.content.links.ff;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;

public class FFBlock extends CimulinkBlock<FFBlockEntity> {

    public static final String ID = "ff";

    public FFBlock(Properties p) {
        super(p);
    }

    @Override
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createFFScreen(p));
    }

    @Override
    public Class<FFBlockEntity> getBlockEntityClass() {
        return FFBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends FFBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.FF_BLOCKENTITY.get();
    }
}
