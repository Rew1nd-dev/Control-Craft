package com.verr1.controlcraft.registry;

import com.tterrag.registrate.util.entry.BlockEntityEntry;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlock;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlock;
import com.verr1.controlcraft.unstable.blocks.monitor.MonitorBlock;
import com.verr1.controlcraft.unstable.blocks.monitor.MonitorBlockEntity;
import com.verr1.controlcraft.unstable.blocks.schematic.SchematicBlock;
import com.verr1.controlcraft.unstable.blocks.schematic.SchematicBlockEntity;


import static com.verr1.controlcraft.ControlCraft.REGISTRATE;

public class AIBlockEntities {

    public static final BlockEntityEntry<CruiserBlockEntity> CRUISER_BLOCKENTITY = REGISTRATE
            .blockEntity(CruiserBlock.ID, CruiserBlockEntity::new)
            .validBlock(AIBlocks.CRUISER)
            .register();

    public static final BlockEntityEntry<AiAttackerBlockEntity> AI_JET_BLOCKENTITY = REGISTRATE
            .blockEntity(AiAttackerBlock.ID, AiAttackerBlockEntity::new)
            .validBlock(AIBlocks.JET)
            .register();

    public static final BlockEntityEntry<MonitorBlockEntity> AI_MONITOR_BLOCKENTITY = REGISTRATE
            .blockEntity(MonitorBlock.ID, MonitorBlockEntity::new)
            .validBlock(AIBlocks.MONITOR)
            .register();

    public static final BlockEntityEntry<SchematicBlockEntity> AI_SCHEMATIC_BLOCKENTITY = REGISTRATE
            .blockEntity(SchematicBlock.ID, SchematicBlockEntity::new)
            .validBlock(AIBlocks.SCHEMATIC)
            .register();

    public static void register() {}
}
