package com.verr1.controlcraft.content.links.ccbridge;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import com.verr1.controlcraft.registry.CimulinkBlocks;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class CCBridgeBlock extends CimulinkBlock<CCBridgeBlockEntity> {

    public static final String ID = "cc_bridge";

    public CCBridgeBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createNameOnlyScreen(p));
    }

    @Override
    public Class<CCBridgeBlockEntity> getBlockEntityClass() {
        return CCBridgeBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends CCBridgeBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.BRIDGE_BLOCKENTITY.get();
    }
}
