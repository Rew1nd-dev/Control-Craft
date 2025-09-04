package com.verr1.controlcraft.content.links.connector;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import com.verr1.controlcraft.registry.CimulinkBlocks;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class EasyConnectorBlock extends CimulinkBlock<EasyConnectorBlockEntity> {
    public static final String ID = "easy_connector";

    public EasyConnectorBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createEasyConnector(p));
    }

    @Override
    public Class<EasyConnectorBlockEntity> getBlockEntityClass() {
        return EasyConnectorBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends EasyConnectorBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.CONNECTOR_BLOCKENTITY.get();
    }
}
