package com.verr1.controlcraft.content.compact.tweak.impl;

import com.getitemfromblock.create_tweaked_controllers.block.ModBlocks;
import com.getitemfromblock.create_tweaked_controllers.block.TweakedLecternControllerBlockEntity;
import com.getitemfromblock.create_tweaked_controllers.item.ModItems;
import com.verr1.controlcraft.content.compact.tweak.ITweakedControllerComponentGetter;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.block.state.BlockState;

public class ITweakedControllerComponentGetterImpl implements ITweakedControllerComponentGetter {

    @Override
    public NamedComponent tweakedControllerPlant(ServerLevel level, BlockPos pos) {
        return BlockEntityGetter.getLevelBlockEntityAt(level, pos, TweakedLecternControllerBlockEntity.class)
                .map(TweakControllerPlant::new)
                .orElse(null);
    }

    @Override
    public BlockState lecternBlock() {
        return ModBlocks.TWEAKED_LECTERN_CONTROLLER.getDefaultState();
    }

    @Override
    public boolean tweakControllerInHand(Player player) {
        return player.getItemInHand(InteractionHand.MAIN_HAND).is(ModItems.TWEAKED_LINKED_CONTROLLER.get());
    }
}
