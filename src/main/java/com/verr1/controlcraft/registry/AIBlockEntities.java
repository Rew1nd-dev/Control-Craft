package com.verr1.controlcraft.registry;

import com.tterrag.registrate.util.entry.BlockEntityEntry;
import com.verr1.controlcraft.content.links.scope.OscilloscopeBlock;
import com.verr1.controlcraft.content.links.scope.OscilloscopeBlockEntity;
import com.verr1.controlcraft.render.OscilloscopeBlockEntityRenderer;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlock;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import com.verr1.controlcraft.unstable.blocks.jet.AiJetBlock;
import com.verr1.controlcraft.unstable.blocks.jet.AiJetBlockEntity;

import static com.verr1.controlcraft.ControlCraft.REGISTRATE;

public class AIBlockEntities {

    public static final BlockEntityEntry<CruiserBlockEntity> CRUISER_BLOCKENTITY = REGISTRATE
            .blockEntity(CruiserBlock.ID, CruiserBlockEntity::new)
            .validBlock(AIBlocks.CRUISER)
            .register();

    public static final BlockEntityEntry<AiJetBlockEntity> AI_JET_BLOCKENTITY = REGISTRATE
            .blockEntity(AiJetBlock.ID, AiJetBlockEntity::new)
            .validBlock(AIBlocks.JET)
            .register();

    public static void register() {}
}
