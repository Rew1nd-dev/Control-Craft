package com.verr1.controlcraft.content.links.mux2;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;

public class Mux2Block extends CimulinkBlock<Mux2BlockEntity> {
    public static final String ID = "mux";

    public Mux2Block(Properties p) {
        super(p);
    }

    @Override
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createNameOnlyScreen(p));
    }

    @Override
    public Class<Mux2BlockEntity> getBlockEntityClass() {
        return Mux2BlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends Mux2BlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.MUX_BLOCKENTITY.get();
    }
}
