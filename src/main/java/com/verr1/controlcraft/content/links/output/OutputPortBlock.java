package com.verr1.controlcraft.content.links.output;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;

public class OutputPortBlock extends CimulinkBlock<OutputPortBlockEntity> {
    public static final String ID = "output_link";

    public OutputPortBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createOutputScreen(p));
    }

    @Override
    public boolean isSignalSource(@NotNull BlockState p_60571_) {
        return true;
    }

    @Override
    public int getSignal(@NotNull BlockState state, @NotNull BlockGetter blockAccess, @NotNull BlockPos pos, @NotNull Direction side) {
        return getBlockEntityOptional(blockAccess, pos).map(OutputPortBlockEntity::getOutputSignal).orElse(0);
    }

    @Override
    public Class<OutputPortBlockEntity> getBlockEntityClass() {
        return OutputPortBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends OutputPortBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.OUTPUT_BLOCKENTITY.get();
    }
}
