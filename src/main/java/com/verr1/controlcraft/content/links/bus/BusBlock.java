package com.verr1.controlcraft.content.links.bus;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class BusBlock extends CimulinkBlock<BusBlockEntity> {
    public static final String ID = "link_bus";

    public BusBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createBusScreen(p));
    }

    @Override
    public Class<BusBlockEntity> getBlockEntityClass() {
        return BusBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends BusBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.BUS_BLOCKENTITY.get();
    }
}
