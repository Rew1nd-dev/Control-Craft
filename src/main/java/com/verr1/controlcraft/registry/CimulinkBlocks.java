package com.verr1.controlcraft.registry;

import com.simibubi.create.foundation.data.BlockStateGen;
import com.simibubi.create.foundation.data.SharedProperties;
import com.simibubi.create.foundation.data.TagGen;
import com.tterrag.registrate.util.entry.BlockEntry;
import com.verr1.controlcraft.content.blocks.loader.ChunkLoaderBlock;
import com.verr1.controlcraft.content.links.ff.FFBlock;
import com.verr1.controlcraft.content.links.logic.LogicGateBlock;
import net.minecraft.world.level.material.MapColor;

import static com.simibubi.create.foundation.data.ModelGen.customItemModel;
import static com.verr1.controlcraft.ControlCraft.REGISTRATE;

public class CimulinkBlocks {

    static {
        REGISTRATE.setCreativeTab(ControlCraftCreativeTabs.MAIN);
    }

    public static final BlockEntry<LogicGateBlock> LOGIC_GATE = REGISTRATE
            .block(LogicGateBlock.ID, LogicGateBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<FFBlock> FF = REGISTRATE
            .block(FFBlock.ID, FFBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static void register(){}
}
