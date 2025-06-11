package com.verr1.controlcraft.content.links.proxy;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;

public class ProxyLinkBlock extends CimulinkBlock<ProxyLinkBlockEntity> {

    public static final String ID = "link_proxy";

    public ProxyLinkBlock(Properties p) {
        super(p);
    }

    @Override
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createProxyScreen(p));
    }

    @Override
    public Class<ProxyLinkBlockEntity> getBlockEntityClass() {
        return ProxyLinkBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends ProxyLinkBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.PROXY_BLOCKENTITY.get();
    }
}
