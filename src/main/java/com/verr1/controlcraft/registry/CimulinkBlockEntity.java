package com.verr1.controlcraft.registry;

import com.tterrag.registrate.util.entry.BlockEntityEntry;
import com.verr1.controlcraft.content.blocks.loader.ChunkLoaderBlock;
import com.verr1.controlcraft.content.blocks.loader.ChunkLoaderBlockEntity;
import com.verr1.controlcraft.content.links.ff.FFBlock;
import com.verr1.controlcraft.content.links.ff.FFBlockEntity;
import com.verr1.controlcraft.content.links.logic.LogicGateBlock;
import com.verr1.controlcraft.content.links.logic.LogicGateBlockEntity;

import static com.verr1.controlcraft.ControlCraft.REGISTRATE;

public class CimulinkBlockEntity {

    public static final BlockEntityEntry<LogicGateBlockEntity> LOGIC_GATE_BLOCKENTITY = REGISTRATE
            .blockEntity(LogicGateBlock.ID, LogicGateBlockEntity::new)
            .validBlock(CimulinkBlocks.LOGIC_GATE)
            .register();

    public static final BlockEntityEntry<FFBlockEntity> FF_BLOCKENTITY = REGISTRATE
            .blockEntity(FFBlock.ID, FFBlockEntity::new)
            .validBlock(CimulinkBlocks.FF)
            .register();

    public static void register(){}
}
