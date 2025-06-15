package com.verr1.controlcraft.content.links.ff;

import com.simibubi.create.content.kinetics.base.DirectionalAxisKineticBlock;
import com.simibubi.create.foundation.gui.ScreenOpener;
import com.tterrag.registrate.providers.DataGenContext;
import com.tterrag.registrate.providers.RegistrateBlockstateProvider;
import com.tterrag.registrate.util.nullness.NonNullBiConsumer;
import com.verr1.controlcraft.content.blocks.spatial.SpatialAnchorBlock;
import com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
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

import java.util.function.BiFunction;
import java.util.function.Function;

import static com.simibubi.create.foundation.data.BlockStateGen.directionalAxisBlock;

public class FFBlock extends CimulinkBlock<FFBlockEntity> {

    public static final String ID = "ff";
    public static final EnumProperty<FFTypes> TYPE = EnumProperty.create("ff_types", FFTypes.class);

    public FFBlock(Properties p) {
        super(p);
    }

    @Override
    public void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(TYPE);
        super.createBlockStateDefinition(builder);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void displayScreen(BlockPos p) {
        ScreenOpener.open(CimulinkUIFactory.createFFScreen(p));
    }

    @Override
    public Class<FFBlockEntity> getBlockEntityClass() {
        return FFBlockEntity.class;
    }

    @Override
    public BlockEntityType<? extends FFBlockEntity> getBlockEntityType() {
        return CimulinkBlockEntities.FF_BLOCKENTITY.get();
    }

    public static class FFDataGenerator {
        public static <T extends DirectionalBlock> NonNullBiConsumer<DataGenContext<Block, T>, RegistrateBlockstateProvider> generate(){
            return
                    (c, p) -> p.directionalBlock(c.get(), modelFunc(c, p));
        }

        private static <T extends DirectionalBlock> Function<BlockState, ModelFile> modelFunc(DataGenContext<Block, T> c, RegistrateBlockstateProvider p){
            return (state) -> {
                FFTypes type = state.getValue(TYPE);

                String name = c.getName();

                String fileName = switch (type){
                    case T_FF, ASYNC_T_FF -> "t";
                    case D_FF, ASYNC_D_FF -> "d";
                    case JK_FF, ASYNC_JK_FF -> "jk";
                    case RS_FF, ASYNC_RS_FF -> "rs";
                };

                return p.models().getExistingFile(p.modLoc("block/" + name + "/" + fileName));
            };
        }

    }
}
