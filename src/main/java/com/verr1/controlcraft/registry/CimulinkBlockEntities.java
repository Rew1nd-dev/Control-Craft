package com.verr1.controlcraft.registry;

import com.tterrag.registrate.util.entry.BlockEntityEntry;
import com.verr1.controlcraft.content.links.ff.FFBlock;
import com.verr1.controlcraft.content.links.ff.FFBlockEntity;
import com.verr1.controlcraft.content.links.input.InputPortBlock;
import com.verr1.controlcraft.content.links.input.InputPortBlockEntity;
import com.verr1.controlcraft.content.links.logic.LogicGateBlock;
import com.verr1.controlcraft.content.links.logic.LogicGateBlockEntity;
import com.verr1.controlcraft.content.links.output.OutputPortBlockEntity;
import com.verr1.controlcraft.content.links.output.OutputPortBlock;

import static com.verr1.controlcraft.ControlCraft.REGISTRATE;

public class CimulinkBlockEntities {

    public static final BlockEntityEntry<LogicGateBlockEntity> LOGIC_GATE_BLOCKENTITY = REGISTRATE
            .blockEntity(LogicGateBlock.ID, LogicGateBlockEntity::new)
            .validBlock(CimulinkBlocks.LOGIC_GATE)
            .register();

    public static final BlockEntityEntry<FFBlockEntity> FF_BLOCKENTITY = REGISTRATE
            .blockEntity(FFBlock.ID, FFBlockEntity::new)
            .validBlock(CimulinkBlocks.FF)
            .register();

    public static final BlockEntityEntry<InputPortBlockEntity> INPUT_BLOCKENTITY = REGISTRATE
            .blockEntity(InputPortBlock.ID, InputPortBlockEntity::new)
            .validBlock(CimulinkBlocks.INPUT)
            .register();

    public static final BlockEntityEntry<OutputPortBlockEntity> OUTPUT_BLOCKENTITY = REGISTRATE
            .blockEntity(OutputPortBlock.ID, OutputPortBlockEntity::new)
            .validBlock(CimulinkBlocks.OUTPUT)
            .register();

    public static void register(){}
}
