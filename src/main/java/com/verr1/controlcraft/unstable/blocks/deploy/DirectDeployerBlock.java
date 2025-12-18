package com.verr1.controlcraft.unstable.blocks.deploy;

import com.verr1.controlcraft.foundation.api.common.ISignalHandler;
import com.verr1.controlcraft.registry.AIBlockEntities;
import com.verr1.controlcraft.unstable.ai.ui.AIUIFactory;
import com.verr1.controlcraft.unstable.blocks.AIBaseBlock;
import net.createmod.catnip.gui.ScreenOpener;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

public class DirectDeployerBlock extends AIBaseBlock<DirectDeployerBlockEntity> implements ISignalHandler {
    public static final String ID = "ai_direct_deployer";

    public DirectDeployerBlock(BlockBehaviour.Properties p) {
        super(p.strength(-1f, 3600000.0f));
    }

    @Override
    public Class<DirectDeployerBlockEntity> getBlockEntityClass() {
        return DirectDeployerBlockEntity.class;
    }

    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p){
        ScreenOpener.open(AIUIFactory.createDirectDeployerScreen(p));
    }



    @Override
    public BlockEntityType<? extends DirectDeployerBlockEntity> getBlockEntityType() {
        return AIBlockEntities.AI_DIRECT_DEPLOYER_BLOCKENTITY.get();
    }

    @Override
    public void accept(Level level, BlockState state, BlockPos pos, Direction direction, int strength) {
        if(strength > 0){
            withBlockEntityDo(level, pos, DirectDeployerBlockEntity::deploy);
        }
    }
}
