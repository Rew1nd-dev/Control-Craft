package com.verr1.controlcraft.content.links.input;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.foundation.api.common.ISignalAcceptor;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class InputPortBlock extends CimulinkBlock<InputPortBlockEntity> implements
        ISignalAcceptor
{

    public static final String ID = "input_link";

    public InputPortBlock(Properties p) {
        super(p);
    }


    @Override
    public void neighborChanged(BlockState state, Level worldIn, BlockPos pos, Block blockIn, BlockPos fromPos,
                                boolean isMoving)  {
        ISignalAcceptor.super.onNeighborChanged(state, worldIn, pos, blockIn, fromPos, isMoving);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createInputScreen(p));
    }

    @Override
    public Class<InputPortBlockEntity> getBlockEntityClass() {
        return InputPortBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends InputPortBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.INPUT_BLOCKENTITY.get();
    }
}
