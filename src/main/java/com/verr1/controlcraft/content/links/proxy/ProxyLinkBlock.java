package com.verr1.controlcraft.content.links.proxy;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

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
    public void neighborChanged(BlockState state, Level world, BlockPos pos, Block otherBlock, BlockPos neighborPos,
                                boolean isMoving) {
        if(world.isClientSide)return;
        Direction direction = Direction.fromDelta(
                neighborPos.getX() - pos.getX(),
                neighborPos.getY() - pos.getY(),
                neighborPos.getZ() - pos.getZ()
        );

        if(direction == null)return;
        if(direction != state.getValue(FACING).getOpposite())return;
        withBlockEntityDo(world, pos, ProxyLinkBlockEntity::updateAttachedPlant);
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
