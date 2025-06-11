package com.verr1.controlcraft.content.links.logic;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class LogicGateBlock extends CimulinkBlock<LogicGateBlockEntity> {
    public static final String ID = "logic_gates";


    public LogicGateBlock(Properties properties) {
        super(properties);
    }


    @Override
    public Class<LogicGateBlockEntity> getBlockEntityClass() {
        return LogicGateBlockEntity.class;
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p){
        ScreenOpener.open(CimulinkUIFactory.createGateScreen(p));
    }


    @Override
    public BlockEntityType<? extends LogicGateBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.LOGIC_GATE_BLOCKENTITY.get();
    }
}
