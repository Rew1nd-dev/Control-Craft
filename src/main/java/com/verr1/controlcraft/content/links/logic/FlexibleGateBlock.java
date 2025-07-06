package com.verr1.controlcraft.content.links.logic;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.tterrag.registrate.providers.DataGenContext;
import com.tterrag.registrate.providers.RegistrateBlockstateProvider;
import com.tterrag.registrate.util.nullness.NonNullBiConsumer;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.model.generators.ModelFile;

import java.util.function.Function;

public class FlexibleGateBlock extends CimulinkBlock<FlexibleGateBlockEntity> {
    public static final String ID = "flexible_gate";

    public static BooleanProperty IS_AND = BooleanProperty.create("is_and");

    public FlexibleGateBlock(Properties p) {
        super(p);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createFlexibleGateScreen(p));
    }

    @Override
    public Class<FlexibleGateBlockEntity> getBlockEntityClass() {
        return FlexibleGateBlockEntity.class;
    }

    @Override
    public void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(IS_AND);
        super.createBlockStateDefinition(builder);
    }

    @Override
    public BlockEntityType<? extends FlexibleGateBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.FLEXIBLE_GATE_BLOCKENTITY.get();
    }

    public static class GateDataGenerator {
        public static <T extends DirectionalBlock> NonNullBiConsumer<DataGenContext<Block, T>, RegistrateBlockstateProvider> generate(){
            return
                    (c, p) -> p.directionalBlock(c.get(), modelFunc(c, p));
        }

        private static <T extends DirectionalBlock> Function<BlockState, ModelFile> modelFunc(DataGenContext<Block, T> c, RegistrateBlockstateProvider p){
            return (state) -> {
                boolean type = state.getValue(IS_AND);
                String name = c.getName();
                String fileName = type ? "and" : "or";
                return p.models().getExistingFile(p.modLoc("block/" + name + "/" + fileName));
            };
        }

    }

}
