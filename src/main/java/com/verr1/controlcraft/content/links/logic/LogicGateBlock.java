package com.verr1.controlcraft.content.links.logic;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.tterrag.registrate.providers.DataGenContext;
import com.tterrag.registrate.providers.RegistrateBlockstateProvider;
import com.tterrag.registrate.util.nullness.NonNullBiConsumer;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.EnumProperty;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.model.generators.ModelFile;

import java.util.function.Function;

public class LogicGateBlock extends CimulinkBlock<LogicGateBlockEntity> {
    public static final String ID = "logic_gates";
    public static final EnumProperty<GateTypes> TYPE = EnumProperty.create("gate_type", GateTypes.class);

    public LogicGateBlock(Properties properties) {
        super(properties);
    }


    @Override
    public Class<LogicGateBlockEntity> getBlockEntityClass() {
        return LogicGateBlockEntity.class;
    }

    @Override
    public void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(TYPE);
        super.createBlockStateDefinition(builder);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p){
        ScreenOpener.open(CimulinkUIFactory.createGateScreen(p));
    }


    @Override
    public BlockEntityType<? extends LogicGateBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.LOGIC_GATE_BLOCKENTITY.get();
    }

    public static class GateDataGenerator {
        public static <T extends DirectionalBlock> NonNullBiConsumer<DataGenContext<Block, T>, RegistrateBlockstateProvider> generate(){
            return
                    (c, p) -> p.directionalBlock(c.get(), modelFunc(c, p));
        }

        private static <T extends DirectionalBlock> Function<BlockState, ModelFile> modelFunc(DataGenContext<Block, T> c, RegistrateBlockstateProvider p){
            return (state) -> {
                GateTypes type = state.getValue(TYPE);

                String name = c.getName();

                String fileName = type.getSerializedName();

                return p.models().getExistingFile(p.modLoc("block/" + name + "/" + fileName));
            };
        }

    }

}
