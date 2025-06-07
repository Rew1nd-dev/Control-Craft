package com.verr1.controlcraft.content.links.comparator;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.ComparatorLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.Mux2LinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;

public class ComparatorBlockEntity extends CimulinkBlockEntity<ComparatorLinkPort> {


    @Override
    protected ComparatorLinkPort create(@NotNull Level level, BlockPos pos) {
        return new ComparatorLinkPort(WorldBlockPos.of(level, pos));
    }

    public ComparatorBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
    }



}
