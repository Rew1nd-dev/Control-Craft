package com.verr1.controlcraft.content.links.output;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;

public class OutputPortBlock extends CimulinkBlock<OutputPortBlockEntity> {
    public static final String ID = "output_link";

    public OutputPortBlock(Properties p) {
        super(p);
    }

    @Override
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createOutputScreen(p));
    }

    @Override
    public Class<OutputPortBlockEntity> getBlockEntityClass() {
        return OutputPortBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends OutputPortBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.OUTPUT_BLOCKENTITY.get();
    }
}
