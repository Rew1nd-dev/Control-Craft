package com.verr1.controlcraft.content.links.shifter;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class ShifterLinkBlock extends CimulinkBlock<ShifterLinkBlockEntity> {

    public static final String ID = "shifter";

    public ShifterLinkBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createShifterScreen(p));
    }

    @Override
    public Class<ShifterLinkBlockEntity> getBlockEntityClass() {
        return ShifterLinkBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends ShifterLinkBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.SHIFTER_BLOCKENTITY.get();
    }
}
