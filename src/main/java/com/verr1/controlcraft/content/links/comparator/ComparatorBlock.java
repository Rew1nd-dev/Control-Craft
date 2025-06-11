package com.verr1.controlcraft.content.links.comparator;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class ComparatorBlock extends CimulinkBlock<ComparatorBlockEntity> {
    public static final String ID = "comparator";

    public ComparatorBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createNameOnlyScreen(p));
    }

    @Override
    public Class<ComparatorBlockEntity> getBlockEntityClass() {
        return ComparatorBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends ComparatorBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.COMPARATOR_BLOCKENTITY.get();
    }
}
