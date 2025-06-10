package com.verr1.controlcraft.content.links.comparator;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.ComparatorLinkPort;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class ComparatorBlockEntity extends CimulinkBlockEntity<ComparatorLinkPort> {


    @Override
    protected ComparatorLinkPort create() {
        return new ComparatorLinkPort();
    }

    public ComparatorBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
    }



}
