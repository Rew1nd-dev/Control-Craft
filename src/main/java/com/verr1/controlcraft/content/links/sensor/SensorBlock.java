package com.verr1.controlcraft.content.links.sensor;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class SensorBlock extends CimulinkBlock<SensorBlockEntity> {

    public static final String ID = "link_sensor";

    public SensorBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createSensorScreen(p));
    }

    @Override
    public Class<SensorBlockEntity> getBlockEntityClass() {
        return SensorBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends SensorBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.SENSOR_BLOCKENTITY.get();
    }
}
