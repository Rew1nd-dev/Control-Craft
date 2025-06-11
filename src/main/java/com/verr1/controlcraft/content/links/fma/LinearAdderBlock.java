package com.verr1.controlcraft.content.links.fma;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class LinearAdderBlock extends CimulinkBlock<LinearAdderBlockEntity> {
    public static final String ID = "fma";

    public LinearAdderBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createFMAScreen(p));
    }

    @Override
    public Class<LinearAdderBlockEntity> getBlockEntityClass() {
        return LinearAdderBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends LinearAdderBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.FMA_BLOCKENTITY.get();
    }
}
