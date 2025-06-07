package com.verr1.controlcraft.content.links.mux2;

import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.Mux2LinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;

public class Mux2BlockEntity extends CimulinkBlockEntity<Mux2LinkPort> {


    public Mux2BlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
    }


    @Override
    protected Mux2LinkPort create(@NotNull Level level, BlockPos pos) {
        return new Mux2LinkPort(WorldBlockPos.of(level, pos));
    }
}
