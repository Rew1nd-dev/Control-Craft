package com.verr1.controlcraft.content.links.scope;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.screens.OscilloscopeScreen;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class OscilloscopeBlock extends CimulinkBlock<OscilloscopeBlockEntity> {
    public static final String ID = "scope";

    public OscilloscopeBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(new OscilloscopeScreen(p));
    }


    @Override
    public Class<OscilloscopeBlockEntity> getBlockEntityClass() {
        return OscilloscopeBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends OscilloscopeBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.SCOPE_BLOCKENTITY.get();
    }
}
