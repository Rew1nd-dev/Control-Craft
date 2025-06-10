package com.verr1.controlcraft.content.links.mux2;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.Mux2LinkPort;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class Mux2BlockEntity extends CimulinkBlockEntity<Mux2LinkPort> {


    public Mux2BlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
    }


    @Override
    protected Mux2LinkPort create() {
        return new Mux2LinkPort();
    }
}
