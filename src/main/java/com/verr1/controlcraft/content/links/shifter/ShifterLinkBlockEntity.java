package com.verr1.controlcraft.content.links.shifter;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.ShifterLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;

public class ShifterLinkBlockEntity extends CimulinkBlockEntity<ShifterLinkPort> {



    public ShifterLinkBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
    }

    @Override
    protected ShifterLinkPort create(@NotNull Level level, BlockPos pos) {
        return new ShifterLinkPort(WorldBlockPos.of(level, pos));
    }
}
