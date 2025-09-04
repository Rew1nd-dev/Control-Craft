package com.verr1.controlcraft.unstable.blocks;

import com.verr1.controlcraft.unstable.valkyrienskies.attachments.AIBlockNetwork;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;

public class AiUtilBlockEntity extends AIBaseBlockEntity{

    public AiUtilBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }



    public void syncSelfIfOnAI(){
        network().ifPresent(n -> n.activateObject(getWorldBlockPos(), this));
    }



    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        syncSelfIfOnAI();
    }
}
