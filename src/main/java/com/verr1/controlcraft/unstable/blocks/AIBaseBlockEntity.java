package com.verr1.controlcraft.unstable.blocks;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.ai.api.IAIListener;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class AIBaseBlockEntity extends OnShipBlockEntity implements
        IAIListener
{

    public AIBaseBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    public void discard(){
        AIServer.MANAGER.discard(getShipOrGroundID());
    }

    public boolean isAI(){
        return AIServer.MANAGER.isAI(getShipOrGroundID());
    }

}
