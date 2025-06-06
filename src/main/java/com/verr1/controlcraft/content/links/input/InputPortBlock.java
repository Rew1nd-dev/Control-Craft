package com.verr1.controlcraft.content.links.input;

import net.createmod.catnip.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;

public class InputPortBlock extends CimulinkBlock<InputPortBlockEntity> {

    public static final String ID = "input_link";

    public InputPortBlock(Properties p) {
        super(p);
    }

    @Override
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
