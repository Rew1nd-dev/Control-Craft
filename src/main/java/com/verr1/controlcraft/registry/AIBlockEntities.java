package com.verr1.controlcraft.registry;

import com.tterrag.registrate.util.entry.BlockEntityEntry;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlockEntity;
import com.verr1.controlcraft.unstable.blocks.autocannon.AiAutoCannonBlock;
import com.verr1.controlcraft.unstable.blocks.autocannon.AiAutocannonBlockEntity;
import com.verr1.controlcraft.unstable.blocks.cannon.AiBigCannonBlock;
import com.verr1.controlcraft.unstable.blocks.cannon.AiBigCannonBlockEntity;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlock;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import com.verr1.controlcraft.unstable.blocks.attacker.AiAttackerBlock;
import com.verr1.controlcraft.unstable.blocks.deploy.DirectDeployerBlock;
import com.verr1.controlcraft.unstable.blocks.deploy.DirectDeployerBlockEntity;
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

    public static final BlockEntityEntry<AiAutocannonBlockEntity> AI_AUTOCANNON_BLOCKENTITY = REGISTRATE
            .blockEntity(AiAutoCannonBlock.ID, AiAutocannonBlockEntity::new)
            .validBlock(AIBlocks.AUTO_CANNON)
            .register();

    public static final BlockEntityEntry<AiBigCannonBlockEntity> AI_BIG_CANNON_BLOCKENTITY = REGISTRATE
            .blockEntity(AiBigCannonBlock.ID, AiBigCannonBlockEntity::new)
            .validBlock(AIBlocks.BIG_CANNON)
            .register();

    public static final BlockEntityEntry<DirectDeployerBlockEntity> AI_DIRECT_DEPLOYER_BLOCKENTITY = REGISTRATE
            .blockEntity(DirectDeployerBlock.ID, DirectDeployerBlockEntity::new)
            .validBlock(AIBlocks.DIRECT_DEPLOYER)
            .register();

    public static void register() {}
}
