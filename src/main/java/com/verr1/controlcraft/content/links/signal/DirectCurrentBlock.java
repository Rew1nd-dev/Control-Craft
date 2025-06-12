package com.verr1.controlcraft.content.links.signal;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class DirectCurrentBlock extends CimulinkBlock<DirectCurrentBlockEntity> {


    protected DirectCurrentBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createDCScreen(p));
    }

    @Override
    public Class<DirectCurrentBlockEntity> getBlockEntityClass() {
        return DirectCurrentBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends DirectCurrentBlockEntity> getBlockEntityType() {
        return null;
    }
}
