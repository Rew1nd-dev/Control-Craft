package com.verr1.controlcraft.registry;

import com.simibubi.create.foundation.data.BlockStateGen;
import com.simibubi.create.foundation.data.SharedProperties;
import com.simibubi.create.foundation.data.TagGen;
import com.tterrag.registrate.util.entry.BlockEntry;
import com.verr1.controlcraft.unstable.blocks.autocannon.AiAutoCannonBlock;
import com.verr1.controlcraft.unstable.blocks.cannon.AiBigCannonBlock;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlock;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlock;
import com.verr1.controlcraft.unstable.blocks.deploy.DirectDeployerBlock;
import com.verr1.controlcraft.unstable.blocks.explosive.ExplosiveBlock;
import com.verr1.controlcraft.unstable.blocks.monitor.MonitorBlock;
import com.verr1.controlcraft.unstable.blocks.schematic.SchematicBlock;
import com.verr1.controlcraft.unstable.blocks.weight.StandardWeightBlock;
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

    public static final BlockEntry<AiAutoCannonBlock> AUTO_CANNON = REGISTRATE
            .block(AiAutoCannonBlock.ID, AiAutoCannonBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(BlockStateGen.directionalBlockProvider(true))
            .item()
            .transform(customItemModel())
            .lang("autocannon")
            .register();

    public static final BlockEntry<AiBigCannonBlock> BIG_CANNON = REGISTRATE
            .block(AiBigCannonBlock.ID, AiBigCannonBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(BlockStateGen.directionalBlockProvider(true))
            .item()
            .transform(customItemModel())
            .lang("big_cannon")
            .register();

    public static final BlockEntry<ExplosiveBlock> EXPLOSIVE = REGISTRATE
            .block(ExplosiveBlock.ID, ExplosiveBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(BlockStateGen.directionalBlockProvider(true))
            .item()
            .transform(customItemModel())
            .lang("war head")
            .register();

    public static final BlockEntry<StandardWeightBlock> WEIGHT = REGISTRATE
            .block(StandardWeightBlock.ID, StandardWeightBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(BlockStateGen.directionalBlockProvider(true))
            .item()
            .transform(customItemModel())
            .lang("100KG")
            .register();

    public static final BlockEntry<DirectDeployerBlock> DIRECT_DEPLOYER = REGISTRATE
            .block(DirectDeployerBlock.ID, DirectDeployerBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(BlockStateGen.directionalBlockProvider(true))
            .item()
            .transform(customItemModel())
            .lang("Direct Deployer")
            .register();

    public static void register(){}

}
