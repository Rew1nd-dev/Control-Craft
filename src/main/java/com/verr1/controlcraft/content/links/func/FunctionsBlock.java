package com.verr1.controlcraft.content.links.func;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class FunctionsBlock extends CimulinkBlock<FunctionsBlockEntity> {

    public static final String ID = "functions";

    public FunctionsBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createFunctionsScreen(p));
    }

    @Override
    public Class<FunctionsBlockEntity> getBlockEntityClass() {
        return FunctionsBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends FunctionsBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.FUNCTIONS_BLOCKENTITY.get();
    }
}
