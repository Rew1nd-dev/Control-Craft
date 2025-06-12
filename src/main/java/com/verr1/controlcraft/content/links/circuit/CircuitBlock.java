package com.verr1.controlcraft.content.links.circuit;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;

public class CircuitBlock extends CimulinkBlock<CircuitBlockEntity> {
    public static final String ID = "circuit";

    public CircuitBlock(Properties p) {
        super(p);
    }

    @Override
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createNameOnlyScreen(p));
    }

    @Override
    public Class<CircuitBlockEntity> getBlockEntityClass() {
        return CircuitBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends CircuitBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.CIRCUIT_BLOCKENTITY.get();
    }
}
