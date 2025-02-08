package com.verr1.vscontrolcraft.blocks.spatialAnchor;

import com.simibubi.create.content.kinetics.base.DirectionalAxisKineticBlock;
import com.tterrag.registrate.providers.DataGenContext;
import com.tterrag.registrate.providers.RegistrateBlockstateProvider;
import com.tterrag.registrate.util.nullness.NonNullBiConsumer;
import com.tterrag.registrate.util.nullness.NonNullBiFunction;
import com.verr1.vscontrolcraft.blocks.sphericalHinge.SphericalHingeBlock;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.client.model.generators.ModelFile;

import static com.simibubi.create.foundation.data.BlockStateGen.directionalAxisBlock;

public class DirectionalAxialFlippableDataGenerator {
    public static <T extends DirectionalAxisKineticBlock> NonNullBiConsumer<DataGenContext<Block, T>, RegistrateBlockstateProvider> generate(){
        return (c, p) -> directionalAxisBlock(c, p, getModelFunc(c, p));
    }

    public static  <T extends Block> NonNullBiFunction<BlockState, Boolean, ModelFile> getModelFunc(DataGenContext<Block, T> context, RegistrateBlockstateProvider prov){
        return
                (blockState, isVertical) ->
                {
                    boolean isFlipped = blockState.getValue(SpatialAnchorBlock.FLIPPED);
                    String vertical = isVertical ? "vertical" : "horizontal";
                    String flipped = isFlipped ? "_flipped" : "";
                    String name = context.getName();
                    return prov.models().getExistingFile(prov.modLoc("block/" + name + "/" + vertical + flipped));
                };
    }
}
