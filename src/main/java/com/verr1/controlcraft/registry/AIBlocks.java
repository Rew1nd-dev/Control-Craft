package com.verr1.controlcraft.registry;

import com.simibubi.create.foundation.data.BlockStateGen;
import com.simibubi.create.foundation.data.SharedProperties;
import com.simibubi.create.foundation.data.TagGen;
import com.tterrag.registrate.util.entry.BlockEntry;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlock;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlock;
import com.verr1.controlcraft.unstable.blocks.monitor.MonitorBlock;
import com.verr1.controlcraft.unstable.blocks.schematic.SchematicBlock;
import net.minecraft.world.level.material.MapColor;

import static com.simibubi.create.foundation.data.ModelGen.customItemModel;
import static com.verr1.controlcraft.ControlCraft.REGISTRATE;

public class AIBlocks {

    static {
        REGISTRATE.setCreativeTab(ControlCraftCreativeTabs.AI);
    }

    public static final BlockEntry<CruiserBlock> CRUISER = REGISTRATE
            .block(CruiserBlock.ID, CruiserBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(BlockStateGen.directionalBlockProvider(true))
            .item()
            .transform(customItemModel())
            .lang("cruiser")
            .register();

    public static final BlockEntry<AiAttackerBlock> JET = REGISTRATE
            .block(AiAttackerBlock.ID, AiAttackerBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(BlockStateGen.directionalBlockProvider(true))
            .item()
            .transform(customItemModel())
            .lang("attacker")
            .register();

    public static final BlockEntry<MonitorBlock> MONITOR = REGISTRATE
            .block(MonitorBlock.ID, MonitorBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(BlockStateGen.directionalBlockProvider(true))
            .item()
            .transform(customItemModel())
            .lang("monitor")
            .register();

    public static final BlockEntry<SchematicBlock> SCHEMATIC = REGISTRATE
            .block(SchematicBlock.ID, SchematicBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(BlockStateGen.directionalBlockProvider(true))
            .item()
            .transform(customItemModel())
            .lang("schematic")
            .register();

    public static void register(){}

}
