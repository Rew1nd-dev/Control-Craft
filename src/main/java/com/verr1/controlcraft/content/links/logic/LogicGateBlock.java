package com.verr1.controlcraft.content.links.logic;

import com.simibubi.create.foundation.block.IBE;
import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.gui.factory.GenericUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.registry.CimulinkBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import static com.verr1.controlcraft.registry.ControlCraftShapes.HALF_BOX_BASE;

public class LogicGateBlock extends CimulinkBlock<LogicGateBlockEntity> {
    public static final String ID = "logic_gates";


    public LogicGateBlock(Properties properties) {
        super(properties);
    }


    @Override
    public Class<LogicGateBlockEntity> getBlockEntityClass() {
        return LogicGateBlockEntity.class;
    }

    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p){
        ScreenOpener.open(CimulinkUIFactory.createLogicGateScreen(p));
    }


    @Override
    public BlockEntityType<? extends LogicGateBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntity.LOGIC_GATE_BLOCKENTITY.get();
    }
}
